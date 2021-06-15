#!/bin/bash

niter=3

for args in `seq 1 3`;
do
    if [ "${args}" -eq "1" ]; then
        two=$(qsub MPI_cycle.PBS -v "args=$args $niter")
    else
        two=$(qsub -W depend=afterany:$one MPI_cycle.PBS -v "args=$args $niter")
    fi
    echo $two
    echo $args $niter
    one=$two
done
