#!/bin/bash
##this work wit:  2nord3.sh taskfile time
#where taskfile contains all tasks to be run by greasy and time the expected time of each task
## warning that the number of task run by greasy depends only of the number of task in the task file AND the number of task by node is hardcoded in proto.job

taskfile=$1
time=$2
ntask=$3


taskdir=`dirname $taskfile`
taskid=`basename $taskfile`
taskid="${taskid%.*}"

sed  -e "s#TASKFILE#$taskfile#" -e "s#TASKDIR#$taskdir#" -e "s/JNAME/$taskid/" -e "s/time=TIME/time=$time/"  -e "s/ntasks=NTASKS/ntasks=$ntask/" proto.job >  $taskdir/$taskid.job



sbatch < $taskdir/$taskid.job



