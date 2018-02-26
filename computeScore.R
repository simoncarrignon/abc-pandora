#script usage: Rscrip --vanilla folder steps
#This script compute a simple score between a simulation and real data
#the result of the simulation have to be in `folder` and the real data in `~/data_per_year.csv`
#`steps` represent the total number of year that will be used to compute the simpson diversity
#score= \frac{\sum_{y}{|simpson(sim_y)-simpson(data_y)|}}{y}


simpsonDiv <- function(x)sum((x/sum(x))^2) #Compute the simpson diversity index as 

computeSimpsonForOneExpe  <-  function(expe,jf=sum,breaks,min)apply(agentWith(expe,breaks=breaks,joinfunction=jf,min=min),1,simpsonDiv) #this compute  the  simson index of the number of settlement with differents good for one experiments


##this function return the number of agent with at least on goods of the goods in the list "goods" and for the timestep in "timestep"
#joinfucntion is the function used to group years put together, it also allow to takes only a subsample of the agents
agentWith <- function(expe,goods=NULL,timestep=NULL,breaks=NULL,joinfunction=sum,min=1,numsite=NULL,bias=NULL){
	print(paste(numsite,bias,breaks))
	
	if(is.null(goods))
		goods=levels(expe$p_good)[which(levels(expe$p_good) != "coins")]
	if(!is.null(breaks))
		expe$timeStep=cut(expe$timeStep,breaks=breaks,label=F)
	if(is.null(timestep))
		timestep=unique(expe$timeStep)
	if(is.null(numsite))
		numsite=rep(numsite,length(timestep))
	if(is.null(numsite))
		numsite=length(levels(expe$agent))
	if(length(numsite)==1){
		numsite=rep(numsite,length(timestep))
		names(numsite)=timestep
	}
	if(length(numsite) != length(timestep) )
		stop("Lenght of break different than length of time")
	else
		names(numsite)=timestep
	expe[is.na(expe)]=0
	cur=expe[expe$p_good == "coins",]
	sapply( timestep, function(tmstp){
	       cur=cur[cur$timeStep == tmstp,] 
	       if(!is.null(bias)){
		       tenpercent=(bias*numsite[tmstp])
		       ranks=cur[cur$timeStep == unique(cur$timeStep)[1],c("agent","size")] #the rank change through time because i implemanted it like that thus I have to recompute the ranks each time. That sucks, and maybe I should force the size of the new consumer to always be low, and here don't have to worry anymore
		       topten=ranks$agent[order(ranks$size,decreasing = T)][1:tenpercent] #when biased toward the big cities we sample the cite but by take at least the 10% of the biggest cities
		       rest=cur$agent[!(cur$agent %in% topten )]
		       random=unique(rest)[round(runif(numsite[tmstp]-tenpercent,1,length(unique(rest))))] 
		       selectedAG=unlist(list(topten,random))

	       }
	       else{
		       ##sampling is random
		       selectedAG=levels(cur$agent)[round(runif(numsite[as.character(tmstp)],1,length(levels(cur$agent))))] 
	       }
	       cur=cur[cur$agent %in% selectedAG,]
	       cur=droplevels(cur)
	       sapply(goods,function(g){

		      if(!is.null(breaks)){#if we want to breaks the dataset in period then we need to put join the timestep of a same period using joinfunction (usually 'sum') 
			      join=tapply(cur[,paste(g,"_q",sep="")],cur$agent,joinfunction)
			      if(min==0)return(length(join[join>=min]))
			      if(min==1)return(length(join[join>min]))
		      }
		      else{ #if not we just count the number of agent with a quatnity > the min
			      if(min==1)return(length(cur[ cur[,paste(g,"_q",sep="")] >= min,"agent"]))
			      if(min==0)return(length(cur[ cur[,paste(g,"_q",sep="")] > min,"agent"]))
		      }
})

})

}


#get the arguments
expDir=commandArgs()[7]
granularity=as.numeric(commandArgs()[8])


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

simpscore <- function(sim,dat) mean(abs(apply(sim,1,simpsonDiv)-apply(dat,1,simpsonDiv)))

zscore <- function(sim,dat) mean(apply((abs(sim-dat)-apply(abs(sim-dat),2,mean))/apply(abs(sim-dat),2,sd),2,mean))



#load simulation data
rawdatasimu=read.csv(file.path(expDir,"agents.csv"),sep=";")
#simu=agentWith(rawdatasimu,min=1,breaks=granularity)
simu=agentWith(rawdatasimu,min=1,numsite = 200 ,breaks=granularity)

#print(paste(" simu: r",nrow(simu)," x c",ncol(simu)))
#print(paste(" realdata: r",nrow(realdata)," x c",ncol(realdata)))
#compute the score
#score=simpscore(simu,realdata)
score=zscore(t(simu),realdata)


print(score)
write(score,file.path(expDir,"score.txt"))

