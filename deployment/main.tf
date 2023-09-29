// This block tells Terraform that we're going to provision AWS resources.

provider "aws" {
  region = "us-east-1"
}

terraform {
  backend "s3" {
    # Replace this with your bucket name!
    bucket         = "serokell-terraform-state"
    key            = "ghc.dev/terraform.tfstate"
    region         = "eu-west-1"
    # Replace this with your DynamoDB table name!
    dynamodb_table = "serokell-terraform-locks"
    encrypt        = true
  }
}
// We'll also need the root domain (also known as zone apex or naked domain).

variable "root_domain_name" {
  default = "ghc.dev"
}

resource "aws_s3_bucket" "www" {
  // Our bucket's name is going to be the same as our site's domain name.
  bucket = "${var.root_domain_name}-prod"
  // Because we want our site to be available on the internet, we set this so
  // anyone can read this bucket.
  acl    = "public-read"
  // We also need to create a policy that allows anyone to view the content.
  // This is basically duplicating what we did in the ACL but it's required by
  // AWS. This post: http://amzn.to/2Fa04ul explains why.
  policy = <<POLICY
{
  "Version":"2012-10-17",
  "Statement":[
    {
      "Sid":"AddPerm",
      "Effect":"Allow",
      "Principal": "*",
      "Action":["s3:GetObject"],
      "Resource":["arn:aws:s3:::${var.root_domain_name}-prod/*"]
    }
  ]
}
POLICY

  // S3 understands what it means to host a website.
  website {
    // Here we tell S3 what to use when a request comes in to the root
    index_document = "index.html"
    // The page to serve up if a request results in an error or a non-existing
    // page.
    //error_document = "404.html"
  }
}

resource "aws_acm_certificate" "cert" {
  domain_name       = var.root_domain_name
  validation_method = "DNS"
}
resource "aws_route53_record" "cert_validation" {
  name    = aws_acm_certificate.cert.domain_validation_options[0].resource_record_name
  type    = aws_acm_certificate.cert.domain_validation_options[0].resource_record_type
  zone_id = aws_route53_zone.zone.zone_id
  records = [aws_acm_certificate.cert.domain_validation_options[0].resource_record_value]
  ttl     = 60
}

resource "aws_acm_certificate_validation" "cert" {
  certificate_arn         = aws_acm_certificate.cert.arn
  validation_record_fqdns = [aws_route53_record.cert_validation.fqdn]
}
resource "aws_cloudfront_distribution" "www_distribution" {
  // origin is where CloudFront gets its content from.
  origin {
    // We need to set up a "custom" origin because otherwise CloudFront won't
    // redirect traffic from the root domain to the www domain, that is from
    // runatlantis.io to www.runatlantis.io.
    custom_origin_config {
      // These are all the defaults.
      http_port              = "80"
      https_port             = "443"
      origin_protocol_policy = "http-only"
      origin_ssl_protocols   = ["TLSv1", "TLSv1.1", "TLSv1.2"]
    }

    // Here we're using our S3 bucket's URL!
    domain_name = aws_s3_bucket.www.website_endpoint

    // This can be any name to identify this origin.
    origin_id = var.root_domain_name
  }

  enabled             = true
  default_root_object = "index.html"

  // All values are defaults from the AWS console.
  default_cache_behavior {
    viewer_protocol_policy = "redirect-to-https"
    compress               = true
    allowed_methods        = ["GET", "HEAD"]
    cached_methods         = ["GET", "HEAD"]
    // This needs to match the `origin_id` above.
    target_origin_id       = var.root_domain_name
    min_ttl                = 0
    default_ttl            = 86400
    max_ttl                = 31536000

    forwarded_values {
      query_string = false
      cookies {
        forward = "none"
      }
    }
  }

  // Here we're ensuring we can hit this distribution using www.runatlantis.io
  // rather than the domain name CloudFront gives us.
  aliases = [var.root_domain_name]

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  // Here's where our certificate is loaded in!
  viewer_certificate {
    acm_certificate_arn = aws_acm_certificate.cert.arn
    ssl_support_method  = "sni-only"
  }
}
// We want AWS to host our zone so its nameservers can point to our CloudFront
// distribution.
resource "aws_route53_zone" "zone" {
  name = var.root_domain_name
}

// This Route53 record will point at our CloudFront distribution.
resource "aws_route53_record" "www" {
  zone_id = aws_route53_zone.zone.zone_id
  name    = var.root_domain_name
  type    = "A"

  alias {
    name                   = aws_cloudfront_distribution.www_distribution.domain_name
    zone_id                = aws_cloudfront_distribution.www_distribution.hosted_zone_id
    evaluate_target_health = false
  }
}

// This Route53 record will point at our CloudFront distribution.
resource "aws_route53_record" "www6" {
  zone_id = aws_route53_zone.zone.zone_id
  name    = var.root_domain_name
  type    = "AAAA"

  alias {
    name                   = aws_cloudfront_distribution.www_distribution.domain_name
    zone_id                = aws_cloudfront_distribution.www_distribution.hosted_zone_id
    evaluate_target_health = false
  }
}
// now a user to deploy this
resource "aws_iam_access_key" "deployer" {
  user = aws_iam_user.deployer.name
}
resource "aws_iam_user" "deployer" {
  name = "${var.root_domain_name}-deployer"
}
resource "aws_iam_user_policy" "deployer" {
  name   = "deploy-${var.root_domain_name}"
  user   = aws_iam_user.deployer.name
  policy = <<POLICY
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "s3:ListBucket",
        "s3:GetBucketLocation"
      ],
      "Resource": "${aws_s3_bucket.www.arn}"
    },
    {
      "Effect": "Allow",
      "Action": [
        "s3:PutObject",
        "s3:PutObjectAcl",
        "s3:GetObject",
        "s3:GetObjectAcl",
        "s3:DeleteObject"
      ],
      "Resource": "${aws_s3_bucket.www.arn}/*"
    },
    {
      "Action": [
        "cloudfront:CreateInvalidation",
        "cloudfront:GetDistribution",
        "cloudfront:GetStreamingDistribution",
        "cloudfront:GetDistributionConfig",
        "cloudfront:GetInvalidation",
        "cloudfront:ListInvalidations",
        "cloudfront:ListStreamingDistributions",
        "cloudfront:ListDistributions"
      ],
      "Effect": "Allow",
      "Resource": "${aws_cloudfront_distribution.www_distribution.arn}"
    }
  ]
}


POLICY
}
output "AWS_ACCESS_KEY_ID" {
  value = aws_iam_access_key.deployer.id
}
output "AWS_SECRET_ACCESS_KEY" {
  value = aws_iam_access_key.deployer.secret
}
output "CDN_DISTRIBUTION_ID" {
  value = aws_cloudfront_distribution.www_distribution.id
}
output "CDN_BUCKET" {
  value = "s3://${aws_s3_bucket.www.bucket}"
}
