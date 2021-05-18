#!/bin/bash

ens=39
niter=2

for args in `seq 1 1`;
do
    if [ "${args}" -eq "1" ]; then
        two=$(qsub MPI_cycle_PSU.PBS -v "args=$args $ens $niter")
    else
        two=$(qsub -W depend=afterany:$one MPI_cycle_PSU.PBS -v "args=$args $ens $niter")
    fi
    echo $two
    echo $args $ens $niter
    one=$two
done
