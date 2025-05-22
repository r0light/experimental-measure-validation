variable "ssk_key_pair_name" {
  type = string
}

variable "subnet_id" {
  type = string
}

variable "security_group_ids" {
  type = list(string)
}

variable "environment" {
  type    = string
  default = "staging"
}

variable "instance_type" {
  type    = string
  default = "t2.xlarge"
}

variable "ami" {
  type    = string
  default = "ami-033fabdd332044f06"
}

variable "jmeter_user" {
  type    = string
  default = "jmeter"
}

variable "jmeter_group" {
  type    = string
  default = "jmeter"
}

variable "ssh_keys_path" {
  type    = list(any)
  default = ["~/.ssh/id_rsa.pub"]
}

variable "jmeter_version" {
  description = "The version of JMeter to install"
  default     = "5.6.3"
  type        = string
}

variable "jmeter_plugins" {
  type        = list(string)
  description = "List of JMeter plugins to install"
  default     = []
  #validation {
  #  condition     = length(var.jmeter_plugins) > 0
  #  error_message = "You must specify at least one JMeter plugin."
  #}
}

variable "jmeter_cmdrunner_version" {
  description = "The version of JMeter CommandRunner to install"
  default     = "2.3"
  type        = string
}
variable "jmeter_plugins_manager_version" {
  description = "The version of JMeter Plugins Manager to install"
  type        = string
  default     = "1.10"
}

variable "jmeter_plan_file" {
  description = "The path to a jmeter test plan file to be uploaded to the jmeter host"
  default     = ""
  type        = string
}
