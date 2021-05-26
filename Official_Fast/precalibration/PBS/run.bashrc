#!/bin/bash

for args in `seq 2 22`;
do
  qsub run.PBS -v "args=$args"
     echo $args
done
