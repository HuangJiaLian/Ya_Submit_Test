#!/bin/bash
#TORQUE -N myjob
#TORQUE -q @node1
cd $PBS_O_WORKDIR
./a.out

