#!/bin/bash

for args in `seq 1 14`;
do
  qsub run_mpi.PBS -v "args=$args"
     echo $args
done
