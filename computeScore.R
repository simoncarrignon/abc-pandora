#script usage: Rscrip --vanilla folder steps diff <- function
#This script compute a simple score between a simulation and real data
#the result of the simulation have to be in `folder` and the real data in `~/data_per_year.csv`
#`steps` represent the total number of year that will be used to compute the simpson diversity
#score= \frac{\sum_{y}{|simpson(sim_y)-simpson(data_y)|}}{y}

source("function.R")

#get the arguments
expDir=commandArgs()[7]
granularity=as.numeric(commandArgs()[8])
diffstr=commandArgs()[9]
pattern=commandArgs()[10]
numsite=as.numeric(commandArgs()[11])

print(diffstr)
difffun = absdiff

if(is.na(numsite) || numsite == "" )
	numsite=NULL

if(!(is.na(diffstr) || diffstr == "" ))
	difffun = get(diffstr) 
#get allows to find the function with a name that match the string in `diffstr`

print(difffun)


#check if the simpson diversity for thoses steps has been already computed
#this file is stored just one folder before which means that we expect the experiemnt to be stored in one folder under the Folder where they are launched. 
#Which is true given the implementation of the classe Experiment in ceec.py (generateState line 103 & initialisation line 54)
#This script as no meaning outside the ABC framework
#In fact this script SHOULD BE implemented as a method eof Experiments, in order to allow the framework to work with any kind of EXPERIMENTS

dataGran=paste0("simpsonData",granularity,".bin")
if(!file.exists(dataGran)){ ##if else to avoid recreate each time very long file
	data=read.csv("~/data_per_year.csv")
	data$goods=data$Fabric
	data$date=cut(data$date,breaks=granularity) 
	realdata=sapply( levels(data$goods) , function(g)sapply(sort(unique(data$date)),function(ts){length(unique(data$Location_ascii[data$date == ts & data$goods == g]))}))
	save(realdata,file=dataGran)
}else{
	load(dataGran)
}




#load simulation data
rawdatasimu=read.csv(file.path(expDir,"agents.csv"),sep=";")
#simu=agentWith(rawdatasimu,min=1,breaks=granularity)
simu=t(agentWith(rawdatasimu,min=1,numsite = numsite ,breaks=granularity,type=pattern))

#print(paste(" simu: r",nrow(simu)," x c",ncol(simu)))
#print(paste(" realdata: r",nrow(realdata)," x c",ncol(realdata)))
#compute the score
#score=simpscore(simu,realdata)

score=difffun(simu,realdata)


print(score)
write(score,file.path(expDir,"score.txt"))

