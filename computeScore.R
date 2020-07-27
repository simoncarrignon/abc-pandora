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

goods=try(as.character(commandArgs()[12]))


if(!is.na(goods))
    if(goods=="noits")goods=c("ESA","ESB","ESC","ESD")
if(is.na(goods))
    goods=NULL

print(commandArgs())

print(diffstr)
print(goods)

if(is.na(numsite) || numsite == "" )
	numsite=NULL

if(!(is.na(diffstr)) || diffstr != "" )
	difffun = get(diffstr) #get allows to find the function with a name that match the string in `diffstr`



#load simulation data
rawdatasimu=read.csv(file.path(expDir,"agents.csv"),sep=";")


#get real data
if(pattern == "both")
{

	simuDis=agentWith(rawdatasimu,min=1,numsite = numsite ,numperiods=granularity,pattern="div",goods=goods)
	simuDiv=agentWith(rawdatasimu,min=1,numsite = numsite ,numperiods=granularity,pattern="dis",goods=goods)

	simuDis[is.na(simuDis)]=0 #that should be useless as this check is done already in agentWith
	simuDiv[is.na(simuDiv)]=0 #that should be useless as this check is done already in agentWith
	realdataDiv=getRealDataCount(numperiods=granularity,proportion=T,pattern="dis",goods=goods)
	realdataDis=getRealDataCount(numperiods=granularity,proportion=T,pattern="div",goods=goods)

	scoreDis=difffun(simuDis,realdataDis)
	scoreDiv=difffun(simuDiv,realdataDiv)
	score=1/2*(scoreDis + scoreDiv)
}else
{

	simu=agentWith(rawdatasimu,min=1,numsite = numsite ,numperiods=granularity,pattern=pattern,goods=goods)

	simu[is.na(simu)]=0 #that should be useless as this check is done already in agentWith
	realdata=getRealDataCount(numperiods=granularity,proportion=T,pattern=pattern,goods=goods)

	score=difffun(simu,realdata)
}

print(score)

write(score,file.path(expDir,"score.txt"))

