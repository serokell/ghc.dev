This dir contains terraform information for the AWS resources that are needed for the ghc.dev hosting.
The state is stored in an aws bucket on our account. Don't forget to set AWS_PROFILE to the correct profile.

From the parent directory:
```bash
export AWS_PROFILE=srk-prod
nix run .#tf-plan
nix run .#tf-apply
```
