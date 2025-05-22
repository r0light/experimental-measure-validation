#!/bin/bash

groupadd ${jmeter_group}
useradd -d /home/${jmeter_user} -r -g ${jmeter_group} ${jmeter_user}
mkdir -p /home/${jmeter_user}/.ssh
touch /home/${jmeter_user}/.ssh/authorized_keys
%{ for ssh_key in ssh_keys }
echo "${ssh_key}" >> /home/${jmeter_user}/.ssh/authorized_keys
%{ endfor }
chown -R ${jmeter_group}:${jmeter_user} /home/${jmeter_user}/
chmod 400 /home/${jmeter_user}/.ssh/authorized_keys

#cat <<EOD >> /etc/ssh/sshd_config 
#Match Group ${jmeter_group}
#   AllowAgentForwarding yes
#   AllowTcpForwarding yes
#   X11Forwarding yes
#   PermitTunnel yes
#   GatewayPorts yes
#   ForceCommand echo 'This account can only be used for ProxyJump (ssh -J)'
#EOD

systemctl restart sshd.service