provider "aws" {
  region = var.region
}

module "private-vpc" {
  region            = var.region
  my_public_ip_cidr = var.my_public_ip_cidr
  vpc_cidr_block    = var.vpc_cidr_block
  environment       = var.environment
  source            = "./modules/private-vpc"
}

output "private_subnets_ids" {
  value = module.private-vpc.private_subnet_ids
}

output "public_subnets_ids" {
  value = module.private-vpc.public_subnet_ids
}

output "security_group_id" {
  value = module.private-vpc.security_group_id
}

output "vpc_id" {
  value = module.private-vpc.vpc_id
}

output "region" {
  value = var.region
}

output "ssh_key_pair_name" {
  value = "${var.ssh_key_pair_name}"
}

module "jmeter-host" {
  ssk_key_pair_name  = var.ssh_key_pair_name
  environment        = var.environment
  subnet_id          = module.private-vpc.public_subnet_ids[0]
  security_group_ids = [module.private-vpc.security_group_id]
  ssh_keys_path      = ["~/.ssh/${var.ssh_key_pair_name}.pub"]
  jmeter_plan_file   = "../c_jmeter/teastore_browse-timed.jmx"
  source             = "./modules/jmeter-host"
}

output "jmeter_host_ip" {
  value = module.jmeter-host.jmeter_host_ip
}


module "eks-cluster-creation" {
  source = "./modules/eks-cluster-creation"
  vpc_id = module.private-vpc.vpc_id
  vpc_private_subnets = module.private-vpc.private_subnet_ids
  vpc_public_subnets = module.private-vpc.public_subnet_ids
}

output "eks-cluster-name" {
  value = module.eks-cluster-creation.cluster_name
}

# Here the output variables from the outputs.tf of the eks-cluster-creation module are inserted wo the load-balancer-controller module
module "load-balancer-controller" {
  source                         = "./modules/load-balancer-controller"
  cluster_arn                    = module.eks-cluster-creation.cluster_arn
  cluster_oidc_issuer_url        = module.eks-cluster-creation.cluster_oidc_issuer_url
  cluster_endpoint               = module.eks-cluster-creation.cluster_endpoint
  cluster_certificate_authority_data = module.eks-cluster-creation.cluster_certificate_authority_data
  cluster_name    = module.eks-cluster-creation.cluster_name
  cluster_version = module.eks-cluster-creation.cluster_version
  cluster_id = module.eks-cluster-creation.cluster_id
  eks_addon_version = module.eks-cluster-creation.eks_addon_version

}

output "autoscaling_group_names" {
  value = module.eks-cluster-creation.eks_managed_node_groups_autoscaling_group_names
}
