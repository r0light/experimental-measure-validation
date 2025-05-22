#!/bin/bash

# parse terraform output to get JMeter host connection details

cd a_provisioning

TERRAFORM_OUTPUT=$(terraform output -json)

JMETER_HOST_IP=$(echo $TERRAFORM_OUTPUT | jq '.["jmeter_host_ip"].value[0]')
JMETER_HOST_IP=$(echo "${JMETER_HOST_IP//[\"\'\`]/}")

SSH_KEY_PAIR_NAME=$(echo $TERRAFORM_OUTPUT | jq '.["ssh_key_pair_name"].value')
SSH_KEY_PAIR_NAME=$(echo "${SSH_KEY_PAIR_NAME//[\"\'\`]/}")

cd ..

# get Load Balancer URL which is the endpoint at which the TeaStore Web UI can be called

echo $(kubectl -n teastore-namespace -ojson get service teastore-webui | jq '.["status"].loadBalancer.ingress[0].hostname')
INTERNAL_LOAD_BALANCER=$(kubectl -n teastore-namespace -ojson get service teastore-webui | jq '.["status"].loadBalancer.ingress[0].hostname' | tr -d '"') 

# copy JMeter plans to JMeter Host

cd c_jmeter
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} teastore_browse-timed.jmx jmeter@${JMETER_HOST_IP}:~/teastore_browse-timed.jmx
cd ..

# prepare script to run the experiment

cat >run_experiment.sh <<XEOF

# get current timestamp as an identifier for this experiment run
TIMESTAMP=\$(date +%Y%m%d_%H%M%S)

# create script that can wait until TeaStore is ready (to be executed on JMeter Host)

cat >check_status.sh <<EOF
STATUS_URL=${INTERNAL_LOAD_BALANCER}/tools.descartes.teastore.webui/status
until \\\$(curl --output /dev/null --silent --head --fail \\\$STATUS_URL); do
  echo "waiting for Server to be ready..."
  sleep 2
done
CURL_OUT=\\\$(curl \\\$STATUS_URL)

while echo \\\$CURL_OUT | grep -q "Offline"
do
    echo "waiting for Teastore to be ready..."
    sleep 10
    CURL_OUT=\\\$(curl \\\$STATUS_URL)
done
echo "Teastore is ready :)"
EOF

# create actual experiment script (to be executed on JMeter Host)

cat >run_remotely.sh <<EOF
export JVM_ARGS="-Xmx14g"

# do warm-up for 10 Minutes with 12000 req/min (200 req/s)
jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=7000 -Jduration=600 -Jthroughput=12000 -t teastore_browse-timed.jmx -l "\${TIMESTAMP}_warmup.csv"


# run experiment 5 times for 5 Minutes with 12000 req/min (200 req/s)
for i in \\\$(seq 1 5);
do
  echo "run \\\${i} out of 5 with 200 req/s"
  jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=7000 -Jduration=300 -Jthroughput=12000 -t teastore_browse-timed.jmx -l \${TIMESTAMP}_200req_\\\${i}_browse_run.csv
done

# run experiment 5 times for 5 Minutes with 9000 req/min (150 req/s)
for i in \\\$(seq 1 5);
do
  echo "run \\\${i} out of 5 with 150 req/s"
  jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=7000 -Jduration=300 -Jthroughput=9000 -t teastore_browse-timed.jmx -l \${TIMESTAMP}_150req_\\\${i}_browse_run.csv
done

# run experiment 5 times for 5 Minutes with 6000 req/min (100 req/s)
for i in \\\$(seq 1 5);
do
  echo "run \\\${i} out of 5 with 100 req/s"
  jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=7000 -Jduration=300 -Jthroughput=6000 -t teastore_browse-timed.jmx -l \${TIMESTAMP}_100req_\\\${i}_browse_run.csv
done
EOF


# wait for TeaStore to be ready
ssh -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP} < ./check_status.sh

# run experiment
ssh -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP} < ./run_remotely.sh

# get resulting data files from JMeter Host
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_warmup.csv ./\${TIMESTAMP}_warmup.csv

for i in \$(seq 1 5);
do
  scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_100req_\${i}_browse_run.csv ./\${TIMESTAMP}_100req_\${i}_browse_run.csv
  scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_150req_\${i}_browse_run.csv ./\${TIMESTAMP}_150req_\${i}_browse_run.csv
  scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_200req_\${i}_browse_run.csv ./\${TIMESTAMP}_200req_\${i}_browse_run.csv
done

XEOF