#! /bin/bash

if [ -f preprocess.sh ];
then
    echo 'preprocess.sh found. Running ...'
    ./preprocess.sh
fi


if [ -f postprocess.sh ];
then
    echo 'postprocess.sh found. Running ...'
    ./postprocess.sh
fi

