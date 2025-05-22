provider "aws" {
  region = var.region
}

locals {
  cluster_name = var.cluster_name
}
locals {
  cluster_version = "1.27"
}

module "eks" {
  source  = "terraform-aws-modules/eks/aws"
  version = "19.15.3"

  cluster_name                    = var.cluster_name
  cluster_version                 = local.cluster_version
  vpc_id                          = var.vpc_id
  subnet_ids                      = var.vpc_private_subnets
  cluster_endpoint_public_access  = true
  cluster_endpoint_private_access = true
  enable_irsa                     = true

  eks_managed_node_group_defaults = {
    ami_type = var.ami_type # Type of Amazon Machine Image (AMI) associated with the EKS Node Group. 
  }

  eks_managed_node_groups = {
    one = {
      name = var.node_group_1_name

      instance_types = [var.default_instance_type]

      min_size     = 1
      max_size     = 10
      desired_size = 6

    }

    two = {
      name = var.node_group_2_name

      instance_types = [var.default_instance_type]

      min_size     = 1
      max_size     = 10
      desired_size = 6
    }
  }
  node_security_group_tags = {
    "karpenter.sh/discovery" = var.cluster_name
  }
}

data "aws_subnets" "private_subnets" {
  filter {
    name   = "vpc-id"
    values = [var.vpc_id]
  }
  tags = {
    Type = "private"
  }
}

data "aws_subnets" "public_subnets" {
  filter {
    name   = "vpc-id"
    values = [var.vpc_id]
  }
  tags = {
    Type = "public"
  }
}

resource "aws_ec2_tag" "k8s_private_cluster_tag" {
  count       = length(var.vpc_private_subnets)
  resource_id = var.vpc_private_subnets[count.index]
  key         = "kubernetes.io/cluster/${var.cluster_name}"
  value       = "owned"
}

resource "aws_ec2_tag" "k8s_private_elb_tag" {
  count       = length(var.vpc_private_subnets)
  resource_id = var.vpc_private_subnets[count.index]
  key         = "kubernetes.io/role/internal-elb"
  value       = "1"
}

resource "aws_ec2_tag" "k8s_private_karpenter_tag" {
  count       = length(var.vpc_private_subnets)
  resource_id = var.vpc_private_subnets[count.index]
  key         = "karpenter.sh/discovery"
  value       = var.cluster_name
}

resource "aws_ec2_tag" "k8s_public_cluster_tag" {
  count       = length(var.vpc_public_subnets)
  resource_id = var.vpc_public_subnets[count.index]
  key         = "kubernetes.io/cluster/${var.cluster_name}"
  value       = "owned"
}

resource "aws_ec2_tag" "k8s_public_elb_tag" {
  count       = length(var.vpc_public_subnets)
  resource_id = var.vpc_public_subnets[count.index]
  key         = "kubernetes.io/role/elb"
  value       = "1"
}

# Elastic Block Store (EBS) Container Storage Interface (CSI) Driver
data "aws_iam_policy" "ebs_csi_policy" {
  arn = "arn:aws:iam::aws:policy/service-role/AmazonEBSCSIDriverPolicy" #This CSI driver enables container orchestrators (such as Kubernetes) to manage the lifecycle of Amazon EBS volumes
}

# creates an IAM role with web identity provider (OIDC) trust and maps the previously retrieved IAM policy (AmazonEBSCSIDriverPolicy) to that role.
module "irsa-ebs-csi" {
  source  = "terraform-aws-modules/iam/aws//modules/iam-assumable-role-with-oidc"
  version = "4.7.0"

  create_role                   = true
  role_name                     = "AmazonEKSTFEBSCSIRole-${module.eks.cluster_name}"
  provider_url                  = module.eks.oidc_provider
  role_policy_arns              = [data.aws_iam_policy.ebs_csi_policy.arn]
  oidc_fully_qualified_subjects = ["system:serviceaccount:kube-system:ebs-csi-controller-sa"]
}

# provides the Amazon EBS CSI driver add-on in the EKS cluster.
resource "aws_eks_addon" "ebs-csi" {
  cluster_name             = module.eks.cluster_name
  addon_name               = "aws-ebs-csi-driver"
  addon_version            = "v1.20.0-eksbuild.1"
  service_account_role_arn = module.irsa-ebs-csi.iam_role_arn
  tags = {
    "eks_addon" = "ebs-csi"
    "terraform" = "true"
  }
}
