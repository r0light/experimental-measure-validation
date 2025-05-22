data "template_cloudinit_config" "jmeter_host" {
  gzip          = true
  base64_encode = true

  part {
    filename     = "init.cfg"
    content_type = "text/cloud-config"
    content      = templatefile("${path.module}/files/cloud-config-base.yaml", {})
  }

  part {
    content_type = "text/x-shellscript"
    content      = templatefile("${path.module}/files/setup_as_bastion.sh", { ssh_keys = local.ssh_keys, jmeter_user = var.jmeter_user, jmeter_group = var.jmeter_group })
  }

  part {
    content_type = "text/x-shellscript"
    content = templatefile("${path.module}/files/install_jmeter.sh",
      {
        JMETER_HOME                         = "/home/${var.jmeter_user}",
        JMETER_VERSION                      = var.jmeter_version,
        JMETER_DOWNLOAD_URL                 = "https://dlcdn.apache.org//jmeter/binaries/apache-jmeter-${var.jmeter_version}.tgz",
        JMETER_CMDRUNNER_VERSION            = var.jmeter_cmdrunner_version,
        JMETER_CMDRUNNER_DOWNLOAD_URL       = "http://search.maven.org/remotecontent?filepath=kg/apc/cmdrunner/${var.jmeter_cmdrunner_version}/cmdrunner-${var.jmeter_cmdrunner_version}.jar",
        JMETER_PLUGINS_MANAGER_VERSION      = var.jmeter_plugins_manager_version,
        JMETER_PLUGINS_MANAGER_DOWNLOAD_URL = "https://repo1.maven.org/maven2/kg/apc/jmeter-plugins-manager/${var.jmeter_plugins_manager_version}/jmeter-plugins-manager-${var.jmeter_plugins_manager_version}.jar",
        JMETER_PLUGINS                      = join(",", var.jmeter_plugins)
    })
  }

  // Caution: there is a 64kB limit for doing it like this:
  part {
    content_type = "text/cloud-config"
    content = yamlencode({
      write_files = [
        {
          encoding    = "b64"
          content     = filebase64("${var.jmeter_plan_file}")
          path        = "/home/${var.jmeter_user}/${basename("${var.jmeter_plan_file}")}"
          owner       = "${var.jmeter_user}:${var.jmeter_group}"
          permissions = "0644"
        }
      ]
    })
  }
}

locals {
  tags = {
    "environment" = "${var.environment}"
  }

  ssh_keys = [for ssh_key in var.ssh_keys_path : file(ssh_key)]
}

resource "aws_instance" "jmeter_host" {
  ami                    = var.ami
  instance_type          = var.instance_type
  key_name               = var.ssk_key_pair_name
  subnet_id              = var.subnet_id
  vpc_security_group_ids = var.security_group_ids

  user_data = data.template_cloudinit_config.jmeter_host.rendered

  tags = merge(
    local.tags,
    {
      Name = "jmeter-host-${var.environment}"
    }
  )

}

