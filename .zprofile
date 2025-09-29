# Added by OrbStack: command-line tools and integration
# This won't be added again if you remove it.
source ~/.orbstack/shell/init.zsh 2>/dev/null || :

timezsh() {
  shell=${1-$SHELL}
  for i in $(seq 1 10); do /usr/bin/time $shell -i -c exit; done
}

# terraform helpers
tfa() {
  workspace=$(terraform workspace show)
  stack=$(basename "$PWD")
  echo "running: terraform apply -var-file ../../config/$workspace/$stack.tfvars $@"
  terraform apply -var-file "../../config/$workspace/$stack.tfvars" $@
}

tfp() {
  workspace=$(terraform workspace show)
  stack=$(basename "$PWD")
  echo "running: terraform plan -var-file ../../config/$workspace/$stack.tfvars $@"
  terraform plan -var-file "../../config/$workspace/$stack.tfvars" $@
}

tfi() {
  workspace=$(terraform workspace show)
  stack=$(basename "$PWD")
  echo "running: terraform import -var-file ../../config/$workspace/$stack.tfvars $@"
  terraform import -var-file "../../config/$workspace/$stack.tfvars" $@
}

tfws() {
  # format of $1 should be $scope-$workspace, e.g. internal-stag
  workspace=$(echo $1 | cut -d- -f2)
  export AWS_PROFILE=$1 && terraform workspace select $workspace
}

