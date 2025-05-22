output "jmeter_host_ip" {
  value = aws_instance.jmeter_host[*].public_ip
}