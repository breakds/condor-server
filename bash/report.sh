#! /bin/bash


# ============= parameters ===============
server=$1
dispatcher=$2
jobid=$3
gap=$4
# ========================================


curl -F "name=${dispatcher}" -F "jobid=${jobid}" -F "data=@console.output" ${server}/report
while sleep ${gap}; do
    curl -F "name=${dispatcher}" -F "jobid=${jobid}" -F "data=@console.output" ${server}/report
done
