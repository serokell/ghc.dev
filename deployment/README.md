This dir contains terraform information for the AWS resources that are needed for the ghc.dev hosting.
The state is stored in an aws bucket on our account. Don't forget to set AWS_PROFILE to the correct profile.

```bash
export AWS_PROFILE=srk-prod
nix develop ../.#tf
terraform init
terraform plan
terraform apply
```
