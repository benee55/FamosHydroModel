#!/bin/bash

for args in `seq 1 58`;
do
  qsub validation.PBS -v "args=$args"
     echo $args
done
