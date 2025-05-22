#!/bin/bash

TIMESTAMP=$(date +%Y%m%d_%H%M%S)

NAMESPACE=teastore-namespace
FILENAME_ALL="${TIMESTAMP}_deployment-topology-all.json"
FILENAME="${TIMESTAMP}_deployment-topology.md"

printf "# K8s Cluster Deployment Topology Dump\n\n" >> $FILENAME
printf "## Nodes\n\n" >> $FILENAME

NODES=$(kubectl get nodes -o json)

printf "{\n\"nodes\":\n" >> $FILENAME_ALL
echo $NODES | jq '.items' >> $FILENAME_ALL
printf "\n" >> $FILENAME_ALL

echo $NODES | sed 's/\\"//g' | jq -c '.items[]' | while read f; do 
     name=$(echo "$f" | jq '.metadata.name'); 
     metadata=$(echo "$f" | jq '.metadata.labels["topology.kubernetes.io/zone"]'); 
     printf "$name: $metadata\n" >> $FILENAME
done

PODS=$(kubectl get po -o json -n $NAMESPACE)

printf "\n,\n\"pods\":\n" >> $FILENAME_ALL
echo $PODS | jq '.items' >> $FILENAME_ALL
printf "}\n" >> $FILENAME_ALL

printf "\n## Pods\n\n" >> $FILENAME

echo $PODS | sed 's/\\"//g' | jq -c '.items[]' | while read f; do 
     name=$(echo "$f" | jq '.metadata.name'); 
     nodeName=$(echo "$f" | jq '.spec.nodeName'); 
     zone=$(echo $NODES | sed 's/\\"//g' | jq '.items[] | select(.metadata.name == '$nodeName') | .metadata.labels["topology.kubernetes.io/zone"]' )
     printf "$name: $nodeName: $zone\n" >> $FILENAME
done

