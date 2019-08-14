This dir contains terraform information for the AWS resources that are needed for the ghc.dev hosting.
The state is stored in an aws bucket on our account.

```bash
nix-shell --run "terraform init && terraform apply"
```
