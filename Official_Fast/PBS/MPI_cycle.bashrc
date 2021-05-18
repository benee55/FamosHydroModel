#!/bin/bash

ens=2015
niter=15

for args in `seq 1 3`;
do
    if [ "${args}" -eq "1" ]; then
        two=$(qsub MPI_cycle.PBS -v "args=$args $ens $niter")
    else
        two=$(qsub -W depend=afterany:$one MPI_cycle.PBS -v "args=$args $ens $niter")
    fi
    echo $two
    echo $args $ens $niter
    one=$two
done
