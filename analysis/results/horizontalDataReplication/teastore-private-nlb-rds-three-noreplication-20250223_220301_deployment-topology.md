# K8s Cluster Deployment Topology Dump

## Nodes

"i-0246d3c1c4d3f8b1b.us-east-2.compute.internal": "us-east-2a"
"i-026d8db45140a2320.us-east-2.compute.internal": "us-east-2b"
"i-050930becc565b4f2.us-east-2.compute.internal": "us-east-2a"
"i-05595423e7c999a38.us-east-2.compute.internal": "us-east-2a"
"i-058927ad95dd1d084.us-east-2.compute.internal": "us-east-2a"
"i-058c42ee0b437c176.us-east-2.compute.internal": "us-east-2b"
"i-05e9858c9b746747f.us-east-2.compute.internal": "us-east-2b"
"i-072261d708765039a.us-east-2.compute.internal": "us-east-2c"
"i-0bc6cae9e6dd93dc5.us-east-2.compute.internal": "us-east-2b"
"i-0d5b7bdb5abf83489.us-east-2.compute.internal": "us-east-2c"
"i-0f075de11fd6646d6.us-east-2.compute.internal": "us-east-2c"
"i-0f763fc1bbf428b57.us-east-2.compute.internal": "us-east-2c"

## Pods

"teastore-auth-848dd67cc7-6prnr": "i-058927ad95dd1d084.us-east-2.compute.internal": "us-east-2a"
"teastore-image-758c554dc-9g6c9": "i-072261d708765039a.us-east-2.compute.internal": "us-east-2c"
"teastore-persistence-5cb4574b98-bpbv6": "i-0bc6cae9e6dd93dc5.us-east-2.compute.internal": "us-east-2b"
"teastore-recommender-5cff6fcd6c-2wvfw": "i-050930becc565b4f2.us-east-2.compute.internal": "us-east-2a"
"teastore-registry-7cd6c95dd4-vmj4k": "i-0f763fc1bbf428b57.us-east-2.compute.internal": "us-east-2c"
"teastore-webui-565dc9497f-j9dd9": "i-05595423e7c999a38.us-east-2.compute.internal": "us-east-2a"

## RDS instances

"maria-db": "us-east-2c"
"first-read-replica": "us-east-2a"
"second-read-replica": "us-east-2b"
