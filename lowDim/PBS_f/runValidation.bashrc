#!/bin/bash

for args in `seq 1 58`;
do
  qsub  -v args=$args validation.PBS
     echo $args
done
