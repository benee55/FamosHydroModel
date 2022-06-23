#!/bin/bash

niter=6
##ens=2015
ens=359
for args in `seq 1 3`;
do
    if [ "${args}" -eq "1" ]; then
        two=$(qsub -v "args=$args $ens $niter" MPI_cycle.PBS )
    else
        two=$(qsub -W depend=afterany:$one -v "args=$args $ens $niter" MPI_cycle.PBS )
    fi
    echo $two
    echo $args $niter $ens 
    one=$two
done
