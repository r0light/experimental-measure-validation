# Instructions

## Prerequisites

**CAUTION**: The experiments are prepared to be run within the AWS cloud. Running the experiments results in costs!

In order to be able to run the scripts included in this folder, you need to install and set up the following tools:

* terraform (<https://developer.hashicorp.com/terraform/install>)
* aws cli (<https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html>)
* jq (<https://jqlang.github.io/jq/download/>)
* kubectl (<https://kubernetes.io/docs/tasks/tools/>)
* helm (<https://helm.sh/docs/intro/install/>)

Prepare the aws cli tool by setting up configuration and credentials so that the tool `aws` can be used afterwards (<https://docs.aws.amazon.com/cli/latest/userguide/getting-started-quickstart.html>). For this you need an AWS account.

## Infrastructure Provisioning

The directory `a_provisioning` contains code to set up a Kubernetes Cluster using a Managed Node Group within the AWS cloud. The setup is based on terraform.

1. If you have not already, generate a SSH key pair which you can use to connect to instances within AWS EC2 and add the generated key to AWS EC2
   (<https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/create-key-pairs.html>)
2. Modify the file `a_provisioning/variables.tf` by setting the variables `my_public_ip_cidr`, `certmanager_email_address`, and `ssh_key_pair_name`. The cidr range will be used to allow access to EC2 instances only from this cidr range. Therefore find out the public ip of the machine that you are using. For the ssh key pair name, set the name of the key from 1.
3. Run the following code to set up terraform within `a_provisioning`:

   ```sh
   terraform init
   terraform plan
   ```

4. If no problems occurred in the previous step, deploy the Kubernetes cluster:

   ```sh
   terraform apply
   ```

5. When the cluster is deployed, run the following command to configure `kubectl` for the newly created cluster:

  ```sh
  aws eks --region us-east-2 update-kubeconfig --name eks-cluster
  ```

   Adapt region and name if required.

**NOTE**: Deploying the cluster may take some time.

## Running Experiments

To prepare the experiment deployments, create a separate namespace once:

```sh
kubectl create ns teastore-namespace
```

Running an experiment generally consists of two steps: 1. Deploy the respective TeaStore application variation 2. Run a load test with jmeter.

The deployment files for the different scenarios are located in `b_deployment`.
Navigate into a corresponding directory and run for example:

```sh
kubectl create -f ./original/teastore-private-nlb-original.yaml
```

This deploys the application to the Kubernetes cluster.

Now you can run the script `prepare_run.sh`. It will generate another script: `run_experiment.sh` which is prepared to be executed and run a load test within the provisioned environment. The result files are copied automatically to the local folder from which the experiment has been started.

**NOTE**: `prepare_run.sh` needs to be executed every time a different deployment of the application is used.
**NOTE**: After a deployment, the TeaStore Application takes some time until it is ready (because sample data is generated and stored). You can check with `curl -L <internal-load-balancer-url>/tools.descartes.teastore.webui/status` whether the application is ready or not before starting the experiment with `run_experiment.sh`.

## Teardown

To tear down the experiment environment, first remove the application and then destroy the Kubernetes cluster. You can use the following commands for example: 

Within an experiment folder:

```sh
kubectl delete -f ./original/teastore-private-nlb-original.yaml

kubectl delete ns teastore-namespace
```

Within `a_provisioning`:

```sh
terraform destroy
```
