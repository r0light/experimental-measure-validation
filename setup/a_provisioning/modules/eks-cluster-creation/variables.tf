variable "region" {
  description = "AWS region"
  type        = string
  default     = "us-east-2"
}

variable "vpc_id" {
  type        = string
  description = "The vpc id"
}

variable "cluster_name" {
  type        = string
  default = "eks-cluster"
  description = "The name of the cluster"
}

variable "node_group_1_name" {
  type        = string
  default = "node-group-1"
  description = "The name of the first node group"
}

variable "node_group_2_name" {
  type        = string
  default = "node-group-2"
  description = "The name of the second node group"
}

variable "default_instance_type" {
  type        = string
  default     = "t3.medium" # 2 vCPUs and 4Gi memory
  description = "Instance type to be used"
}

variable "ami" {
  type    = string
  default = "ami-024ebc7de0fc64e44"
}

variable "ami_type" {
  type = string
  default = "AL2_x86_64"
}

variable "vpc_private_subnets" {
  type        = list(string)
  description = "The private vpc subnets ids"
}

variable "vpc_public_subnets" {
  type        = list(string)
  description = "The public vpc subnets ids"
}