#!/bin/bash

# make sure these are aligned with ../preparation/rds_deployment.sh
ROUTE53_HOSTED_ZONE="teadb-rds.com"
RDS_INSTANCE_NAME="maria-db"

HOSTED_ZONE_ID=$(cat ../preparation/hosted-zone-output.json | jq '.["HostedZone"].Id')
HOSTED_ZONE_ID=$(echo "${HOSTED_ZONE_ID//[\"\'\`]/}")

DB_INSTANCE_DNS_NAME=$(aws rds describe-db-instances --db-instance-identifier "${RDS_INSTANCE_NAME}" | jq '.["DBInstances"][0].Endpoint["Address"]')
# alternative: kubectl -n teastore-namespace -ojson get DBInstance maria-db


cat <<EOF > record-sets.json
{
            "Comment": "CREATE a record ",
            "Changes": [{
            "Action": "CREATE",
                        "ResourceRecordSet": {
                                    "Name": "instance.${ROUTE53_HOSTED_ZONE}",
                                    "Type": "CNAME",
                                    "TTL": 300,
                                 "ResourceRecords": [{ "Value": ${DB_INSTANCE_DNS_NAME}}]
}}]
}
EOF


aws route53 change-resource-record-sets --hosted-zone-id "${HOSTED_ZONE_ID}" --change-batch file://record-sets.json