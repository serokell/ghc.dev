{ ... }:
let
  root_domain_name = "ghc.dev";
in
{
  provider = {
    aws = {
      region = "us-east-1";
    };
  };

  terraform = {
    required_version = ">= 1.5.7";
    required_providers = {
      aws = {
        source = "hashicorp/aws";
        version = "= 5.19.0";
      };
    };
    backend."s3"= {
      bucket         = "serokell-terraform-state";
      key            = "ghc.dev/terraform.tfstate";
      region         = "eu-west-1";
      dynamodb_table = "serokell-terraform-locks";
      encrypt        = true;
    };
  };

  resource = {

    aws_s3_bucket.www = {
      # Our bucket's name is going to be the same as our site's domain name.
      bucket = "${root_domain_name}-prod";
    };

    aws_s3_bucket_acl.example = {
      bucket = "\${aws_s3_bucket.www.id}";

      # Because we want our site to be available on the internet, we set this so
      # anyone can read this bucket.
      acl    = "public-read";
    };

    aws_s3_bucket_website_configuration.www = {
      bucket = "\${aws_s3_bucket.www.id}";

      index_document = {
        suffix = "index.html";
      };
    };

    aws_s3_bucket_server_side_encryption_configuration.www = {
      bucket = "\${aws_s3_bucket.www.id}";

      rule = {
        apply_server_side_encryption_by_default = {
          sse_algorithm = "AES256";
        };
      };
    };

    aws_s3_bucket_policy.www_allow_access = {
      bucket = "\${aws_s3_bucket.www.id}";
      # We also need to create a policy that allows anyone to view the content.
      # This is basically duplicating what we did in the ACL but it's required by
      # AWS. This post: http://amzn.to/2Fa04ul explains why.
      policy = builtins.toJSON {
        Version = "2012-10-17";
        Statement = [
          {
          Sid = "AddPerm";
          Effect = "Allow";
          Principal = "*";
          Action = ["s3:GetObject"];
          Resource = ["arn:aws:s3:::${root_domain_name}-prod/*"];
          }
        ];
      };
    };

    aws_acm_certificate.cert = {
      domain_name       = root_domain_name;
      validation_method = "DNS";
    };

    aws_route53_record.cert_validation = {
      for_each = ''''${{
        for dvo in aws_acm_certificate.cert.domain_validation_options : dvo.domain_name  => {
          name    = dvo.resource_record_name
          record  = dvo.resource_record_value
          type    = dvo.resource_record_type
        }
      }}'';

      name    = "\${each.value.name}";
      type    = "\${each.value.type}";
      zone_id = "\${aws_route53_zone.zone.zone_id}";
      records = ["\${each.value.record}"];
      ttl     = 60;
    };

    aws_acm_certificate_validation.cert = {
      certificate_arn         = "\${aws_acm_certificate.cert.arn}";
      validation_record_fqdns = "\${[for record in aws_route53_record.cert_validation : record.fqdn]}";
    };

    aws_cloudfront_distribution.www_distribution = {
      # origin is where CloudFront gets its content from.
      origin = {
        # We need to set up a "custom" origin because otherwise CloudFront won't
        # redirect traffic from the root domain to the www domain, that is from
        # runatlantis.io to www.runatlantis.io.
        custom_origin_config = {
          # These are all the defaults.
          http_port              = "80";
          https_port             = "443";
          origin_protocol_policy = "http-only";
          origin_ssl_protocols   = ["TLSv1" "TLSv1.1" "TLSv1.2"];
        };

        # Here we're using our S3 bucket's URL!
        domain_name = "\${aws_s3_bucket_website_configuration.www.website_endpoint}";

        # This can be any name to identify this origin.
        origin_id = root_domain_name;
      };

      enabled             = true;
      default_root_object = "index.html";

      # All values are defaults from the AWS console.
      default_cache_behavior = {
        viewer_protocol_policy = "redirect-to-https";
        compress               = true;
        allowed_methods        = ["GET" "HEAD"];
        cached_methods         = ["GET" "HEAD"];
        # This needs to match the `origin_id` above.
        target_origin_id       = root_domain_name;
        min_ttl                = 0;
        default_ttl            = 86400;
        max_ttl                = 31536000;

        forwarded_values = {
          query_string = false;
          cookies = {
            forward = "none";
          };
        };
      };

      # Here we're ensuring we can hit this distribution using www.runatlantis.io
      # rather than the domain name CloudFront gives us.
      aliases = [root_domain_name];

      restrictions = {
        geo_restriction = {
          restriction_type = "none";
        };
      };

      # Here's where our certificate is loaded in!
      viewer_certificate = {
        acm_certificate_arn = "\${aws_acm_certificate.cert.arn}";
        ssl_support_method  = "sni-only";
      };
    };

    # We want AWS to host our zone so its nameservers can point to our CloudFront
    # distribution.
    aws_route53_zone.zone = {
      name = root_domain_name;
    };

    # This Route53 record will point at our CloudFront distribution.
    aws_route53_record.www = {
      zone_id = "\${aws_route53_zone.zone.zone_id}";
      name    = root_domain_name;
      type    = "A";

      alias = {
        name                   = "\${aws_cloudfront_distribution.www_distribution.domain_name}";
        zone_id                = "\${aws_cloudfront_distribution.www_distribution.hosted_zone_id}";
        evaluate_target_health = false;
      };
    };

    # This Route53 record will point at our CloudFront distribution.
    aws_route53_record.www6 = {
      zone_id = "\${aws_route53_zone.zone.zone_id}";
      name    = root_domain_name;
      type    = "AAAA";

      alias = {
        name                   = "\${aws_cloudfront_distribution.www_distribution.domain_name}";
        zone_id                = "\${aws_cloudfront_distribution.www_distribution.hosted_zone_id}";
        evaluate_target_health = false;
      };
    };

    # now a user to deploy this
    aws_iam_access_key.deployer = {
      user = "\${aws_iam_user.deployer.name}";
    };

    aws_iam_user.deployer = {
      name = "${root_domain_name}-deployer";
    };

    aws_iam_user_policy.deployer = {
      name   = "deploy-${root_domain_name}";
      user   = "\${aws_iam_user.deployer.name}";
      policy = builtins.toJSON {
        Version = "2012-10-17";
        Statement = [
          {
            Effect = "Allow";
            Action = [
              "s3:ListBucket"
              "s3:GetBucketLocation"
            ];
            Resource = "\${aws_s3_bucket.www.arn}";
          }
          {
            Effect = "Allow";
            Action = [
              "s3:PutObject"
              "s3:PutObjectAcl"
              "s3:GetObject"
              "s3:GetObjectAcl"
              "s3:DeleteObject"
            ];
            Resource = "\${aws_s3_bucket.www.arn}/*";
          }
          {
            Action = [
              "cloudfront:CreateInvalidation"
              "cloudfront:GetDistribution"
              "cloudfront:GetStreamingDistribution"
              "cloudfront:GetDistributionConfig"
              "cloudfront:GetInvalidation"
              "cloudfront:ListInvalidations"
              "cloudfront:ListStreamingDistributions"
              "cloudfront:ListDistributions"
            ];
            Effect = "Allow";
            Resource = "\${aws_cloudfront_distribution.www_distribution.arn}";
          }
        ];
      };
    };
  };

  output = {
    AWS_ACCESS_KEY_ID = {
        value = "\${aws_iam_access_key.deployer.id}";
    };
    AWS_SECRET_ACCESS_KEY = {
      value = "\${nonsensitive(aws_iam_access_key.deployer.secret)}";
    };
    CDN_DISTRIBUTION_ID = {
      value = "\${aws_cloudfront_distribution.www_distribution.id}";
    };
    CDN_BUCKET = {
      value = "s3://\${aws_s3_bucket.www.bucket}";
    };
  };
}
