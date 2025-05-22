#!/bin/bash


APP_NAMESPACE=teastore-namespace
RDS_INSTANCE_NAME="maria-db"
RDS_SECURITY_GROUP_NAME="sgmariadb"


kubectl delete secret "${RDS_INSTANCE_NAME}-password"

kubectl delete -f ../preparation/rds-mariadb.yaml
kubectl delete -f ../preparation/db-subnet-groups.yaml


HOSTED_ZONE_ID=$(cat ../preparation/hosted-zone-output.json | jq '.["HostedZone"].Id')
aws route53 delete-hosted-zone --id "${HOSTED_ZONE_ID}"

aws ec2 delete-security-group --group-name "${RDS_SECURITY_GROUP_NAME}"