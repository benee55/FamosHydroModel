#!/bin/bash

for args in `seq 1 1`;
do
  qsub run.PBS -v "args=$args"
     echo $args
done