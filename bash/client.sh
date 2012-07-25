#! /bin/bash

# need to fill those variables
# ======================================================================
server=http://emperor.cs.wisc.edu:4242
dispatcher=first
let gap=60
# ======================================================================

if [ -f preprocess.sh ];
then
    echo 'preprocess.sh found. Running ...'
    ./preprocess.sh ${dispatcher}
fi



# register slot
response=($(curl --silent --data "name=${dispatcher}" ${server}/register))
if [[ ${response[0]} = "ok" ]]; then
    len=${#response[@]}
    let maxsub=len-1
    # download shared files
    for i in $(seq ${maxsub}); do
        rm -f ${response[$i]}
        wget ${server}/${dispatcher}/shared/${response[$i]}
        chmod +x ${response[$i]}
    done
fi


response=($(curl --silent --data "name=${dispatcher}" ${server}/fetch))
until [[ ${response[0]} -eq -1 ]]; do
    
    jobid=${response[0]}
    
    # wget the job-specific input files
    len=${#response[@]}
    let maxsub=len-1
    for i in $(seq ${maxsub}); do
        rm -f ${response[$i]}
        wget ${server}/${dispatcher}/input/${jobid}/${response[$i]}
        chmod +x ${response[$i]}
    done
    
    rm -f console.output
    eval "./runprocess.sh&" &> console.output
    pid=$!

    while sleep ${gap}; do
        kill -0 ${pid}
        if [[ $? -eq 0 ]]; then
            # report to server
            curl -F "name=${dispatcher}" -F "jobid=${jobid}" -F "data=@console.output" ${server}/report
        else
            break
        fi
    done
    # last report
    curl -F "name=${dispatcher}" -F "jobid=${jobid}" -F "data=@console.output" ${server}/report
    
    # signal complete
    if [[ $(tar -tvf output.tar.gz | wc -l) -eq 0 ]]; then
        curl --data "name=${dispatcher}&jobid=${jobid}" ${server}/sigfailure
    else
        curl --data "name=${dispatcher}&jobid=${jobid}" ${server}/sigcomplete
        # send output back
        if [ -f output.tar.gz ]; then
            curl -F "name=${dispatcher}" -F "jobid=${jobid}" -F "data=@output.tar.gz" ${server}/upload
        fi
    fi

    


    # clean-up
    for i in $(seq ${maxsub}); do
        rm -f ${response[$i]}
    done
    
    response=($(curl --silent --data "name=${dispatcher}" ${server}/fetch))
done


if [ -f postprocess.sh ];
then
    echo 'postprocess.sh found. Running ...'
    ./postprocess.sh
fi

