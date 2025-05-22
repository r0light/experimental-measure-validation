variable "region" {
  description = "AWS region"
  type        = string
  default     = "us-east-2"
}

variable "environment" {
  description = "An identifier for the current environment to separate deployments"
  type        = string
  default     = "staging"
}

variable "my_public_ip_cidr" {
  description = "A CIDR range to allow access to the bastion host via SSH"
  type        = string
  default     = "" # Set a CIDR range here that includes your local public IP
}

variable "vpc_cidr_block" {
  description = "A CIDR range to allow access between hosts of the private subnet"
  type        = string
  default     = "172.68.0.0/16"
}

variable "certmanager_email_address" {
  description = "An email address which is used for certificate generation"
  type        = string
  default     = "" # Set your email address here
}

variable "ssh_key_pair_name" {
  description = "The name of an existing SSH key pair which is used to access the bastion host"
  type        = string
  default     = "" # set the name of an ssh key pair here that you have added to EC2 so that you can access the jmeter Host via SSH
}