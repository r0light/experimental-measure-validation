#!/bin/bash

cd ../../../a_provisioning

TERRAFORM_OUTPUT=$(terraform output -json)

AWS_REGION=$(echo $TERRAFORM_OUTPUT | jq '.["region"].value')
AWS_REGION=$(echo "${AWS_REGION//[\"\'\`]/}")

VPC_ID=$(echo $TERRAFORM_OUTPUT | jq '.["vpc_id"].value')
VPC_ID=$(echo "${VPC_ID//[\"\'\`]/}")

EKS_CLUSTER_NAME=$(echo $TERRAFORM_OUTPUT | jq '.["eks-cluster-name"].value')
EKS_CLUSTER_NAME=$(echo "${EKS_CLUSTER_NAME//[\"\'\`]/}")

EKS_SUBNET_IDS=$(echo $TERRAFORM_OUTPUT | jq -j '.["private_subnets_ids"].value | join("\n")')

cd ../b_deployment/horizontalDataReplication/preparation



RDS_SUBNET_GROUP_NAME="mariadb-subnets"
RDS_SUBNET_GROUP_DESCRIPTION="private subnets from EKS cluster"
RDS_SECURITY_GROUP_NAME="sgmariadb"
RDS_SECURITY_GROUP_DESCRIPTION="allows traffic to db"
RDS_INSTANCE_NAME="maria-db"
DB_INSTANCE_CLASS="db.t4g.medium"

APP_NAMESPACE=teastore-namespace

EKS_VPC_ID=$(aws eks describe-cluster --name="${EKS_CLUSTER_NAME}" \
  --query "cluster.resourcesVpcConfig.vpcId" \
  --output text)

echo 

cat <<-EOF > db-subnet-groups.yaml
apiVersion: rds.services.k8s.aws/v1alpha1
kind: DBSubnetGroup
metadata:
  name: ${RDS_SUBNET_GROUP_NAME}
  namespace: ${APP_NAMESPACE}
spec:
  name: ${RDS_SUBNET_GROUP_NAME}
  description: ${RDS_SUBNET_GROUP_DESCRIPTION}
  subnetIDs:
$(printf "    - %s\n" ${EKS_SUBNET_IDS})
  tags: []
EOF

kubectl apply -f db-subnet-groups.yaml



EKS_VPC_ID=$(aws eks describe-cluster --name="${EKS_CLUSTER_NAME}" \
  --query "cluster.resourcesVpcConfig.vpcId" \
  --output text)

EKS_CIDR_RANGE=$(aws ec2 describe-vpcs \
  --vpc-ids $EKS_VPC_ID \
  --query "Vpcs[].CidrBlock" \
  --output text
)

RDS_SECURITY_GROUP_ID=$(aws ec2 create-security-group \
  --group-name "${RDS_SECURITY_GROUP_NAME}" \
  --description "${RDS_SECURITY_GROUP_DESCRIPTION}" \
  --vpc-id "${EKS_VPC_ID}" \
  --output text
)

aws ec2 authorize-security-group-ingress \
  --group-id "${RDS_SECURITY_GROUP_ID}" \
  --protocol tcp \
  --port 3306 \
  --cidr "${EKS_CIDR_RANGE}"

kubectl create secret generic "${RDS_INSTANCE_NAME}-password" \
  --from-literal=password="teapassword"

cat <<EOF > rds-mariadb.yaml
apiVersion: rds.services.k8s.aws/v1alpha1
kind: DBInstance
metadata:
  name: "${RDS_INSTANCE_NAME}"
  namespace: "teastore-namespace"
spec:
  allocatedStorage: 20
  dbInstanceClass: "${DB_INSTANCE_CLASS}"
  dbInstanceIdentifier: "${RDS_INSTANCE_NAME}"
  dbName: "teadb"
  dbSubnetGroupName: "${RDS_SUBNET_GROUP_NAME}"
  vpcSecurityGroupIDs:
  - "${RDS_SECURITY_GROUP_ID}"
  engine: mariadb
  engineVersion: "10.6"
  multiAZ: true
  masterUsername: "teauser"
  masterUserPassword:
    namespace: default
    name: "${RDS_INSTANCE_NAME}-password"
    key: password
EOF

## Create DB
kubectl apply -f rds-mariadb.yaml



ROUTE53_HOSTED_ZONE="teadb-rds.com"

# Create Route53 Hosted zone
aws route53 create-hosted-zone \
  --name "${ROUTE53_HOSTED_ZONE}" \
  --caller-reference $(date +%Y%m%d_%H%M%S) \
  --vpc VPCRegion="${AWS_REGION}",VPCId="${VPC_ID}" \
  --hosted-zone-config Comment="private zone for rds access",PrivateZone=true \
  --output json > hosted-zone-output.json