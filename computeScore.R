#script usage: Rscrip --vanilla folder steps diff <- function
#This script compute a simple score between a simulation and real data
#the result of the simulation have to be in `folder` and the real data in `~/data_per_year.csv`
#`steps` represent the total number of year that will be used to compute the simpson diversity
#score= \frac{\sum_{y}{|simpson(sim_y)-simpson(data_y)|}}{y}

#this file is stored just one folder before which means that we expect the experiemnt to be stored in one folder under the Folder where they are launched. 
#Which is true given the implementation of the classe Experiment in ceec.py (generateState line 103 & initialisation line 54)
#This script as no meaning outside the ABC framework
#In fact this script SHOULD BE implemented as a method eof Experiments, in order to allow the framework to work with any kind of EXPERIMENTS

source("function.R")

#get the arguments
expDir=commandArgs()[7]
granularity=as.numeric(commandArgs()[8])
diffstr=commandArgs()[9]
pattern=as.character(commandArgs()[10])
numsite=as.numeric(commandArgs()[11])
print(commandArgs())

print(diffstr)

if(is.na(numsite) || numsite == "" )
	numsite=NULL

if(!(is.na(diffstr)) || diffstr != "" )
	difffun = get(diffstr) #get allows to find the function with a name that match the string in `diffstr`



#load simulation data
rawdatasimu=read.csv(file.path(expDir,"agents.csv"),sep=";")

simu=agentWith(rawdatasimu,min=1,numsite = numsite ,numperiods=granularity,pattern=pattern)

simu[is.na(simu)]=0 #that should be useless as this check is done already in agentWith

#get real data
realdata=getRealDataCount(numperiods=granularity,proportion=T,pattern=pattern)


score=difffun(simu,realdata)

print(score)

write(score,file.path(expDir,"score.txt"))

