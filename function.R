simpsonDiv <- function(x)sum((x/sum(x))^2) #Compute the simpson diversity index as 

##return proportions 
getprop  <- function(x)x/apply(x,1,sum)

simpscore <- function(sim,dat) mean(abs(apply(sim,1,simpsonDiv)-apply(dat,1,simpsonDiv)))

zscore <- function(sim,dat){abs(mean(apply((abs(sim-dat)-apply(abs(sim-dat),2,mean))/apply(abs(sim-dat),2,sd),2,mean)))}

absdiff <- function(sim,dat){mean(apply(abs(sim-dat),2,mean))}


computeSimpsonForOneExpe  <-  function(expe,jf=sum,breaks,min)apply(agentWith(expe,breaks=breaks,joinfunction=jf,min=min),1,simpsonDiv) #this compute  the  simson index of the number of settlement with differents good for one experiments


getSample  <- function(origin,ranks,bias,subsize){
	subsample=c()
	nfixedsites=round((bias*min(subsize,length(origin)))) #number of site selected for their size
	nrandomsites=round(min(subsize,length(origin))-nfixedsites)  #number of site randomly selected

	fixed=droplevels(ranks$agent[order(ranks$size,decreasing = T)][1:nfixedsites]) #when biased toward the big cities we sample the cite but by take at least the 10% of the biggest cities
	if(nrandomsites == 0){
		subsample=fixed
	}
	else {
		rest=origin[!(origin %in% fixed )] #the agents from which we will randomly select
		random=rest[sample.int(length(rest),nrandomsites)] 
		if (nfixedsites == 0)
			subsample=random
		else
			subsample=unlist(list(fixed,random))
	}
	if(length(subsample)==0)stop("probleme while sampling")
	if(length(subsample)<subsize)warnings("impossible to reach asked samplesize")
	return(subsample)
}


##this function return the number of agent with at least on goods of the goods in the list "goods" and for the timestep in "timestep"
#joinfucntion is the function used to group years put together, it also allow to takes only a subsample of the agents
#goods = the goods that will be counted
#timestep = the timestep used ie the interval between wich the sum (or ainy other `joinfunction`) will be counted
#breaks = the timestep used 
#numsite = if we are NOT using all the sites, one can: determine a number of site or give vector of number of site that will be used for each timestep
#bias, if using a number of site < of the total number of agents then this allow to fix a percentage of the bigger sites that will be allways used
agentWith <- function(expe,goods=NULL,timestep=NULL,breaks=NULL,joinfunction=sum,min=1,numsite=NULL,bias=1,type="div"){
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
	cur=expe[expe$p_good == "coins",] #keep only the consumers (ie people producing "coins")

	sapply( timestep, function(tmstp){
	       tmstp=as.character(tmstp)
	       cur=cur[cur$timeStep == tmstp,] 

	       origin=droplevels(unique(cur$agent))
	       ranks=unique(cur[cur$timeStep == unique(cur$timeStep)[1],c("agent","size")]) #the rank change through time because i implemanted it like that thus I have to recompute the ranks each time. That sucks, and maybe I should force the size of the new consumer to always be low, and here don't have to worry anymore
	       ranks=ranks[ranks$agent %in% origin,]
	       selectedAG=getSample(origin,ranks,bias,numsite[tmstp])
	       cur=cur[cur$agent %in% selectedAG,]
	       cur=droplevels(cur)

	       if(type=="div"){
			countype = sapply(selectedAG,function(ag){
			      if(min==0)sum(apply(cur[cur$agent == ag,paste0(goods,"_q") ] ,2,sum)>0)
			      if(min==1)sum(apply(cur[cur$agent == ag,paste0(goods,"_q") ] ,2,sum)>=min)
})
	       return(table(factor(countype,levels=0:length(goods))))

	       }
	       if(type=="dis"){
		       sapply(goods,function(g){

			      if(!is.null(breaks)){#if we want to breaks the dataset in period then we need to put join the timestep of a same period using joinfunction (usually 'sum') 
				      join=tapply(cur[,paste(g,"_q",sep="")],cur$agent,joinfunction)
				      if(min==0)return(length(join[join>min]))
				      if(min==1)return(length(join[join>=min]))
			      }
			      else{ #if not we just count the number of agent with a quantity > the min
				      if(min==0)return(length(cur[ cur[,paste(g,"_q",sep="")] > min,"agent"]))
				      if(min==1)return(length(cur[ cur[,paste(g,"_q",sep="")] >= min,"agent"]))
			      }
})
	       }

})

}
diffDataYear <- function(y,e,d,prop=T,diff=absdiff,numsite=40,type="div"){
	tryCatch(
		 {
			 print(type)

			 dt=t(agentWith(e$data,breaks=y,numsite=numsite,type=type))
			 rdt=d[[as.character(y)]]
			 print(ncol(dt))
			 print(ncol(rdt))
			 if(prop){
				 dt=getprop(dt)
				 rdt=getprop(rdt)
			 }
			 return(diff(dt,rdt))},error=function(err){NA})
}


#datalist=list with all data
#years list of years bininb
#diff function to comput distance
#pattern realdata to ompare 
#par=T true if to be parallelized
#prop=T true if to use proportions
getAllScores <- function(datalist,years,diff,pattern,par=T,prop=T){
	res=list()
	type=""
	if(length(grep("div",deparse(substitute(pattern))))>0)
		type="div"
	else
		type="dis"


	if(par){
		cl <- makeCluster(detectCores(),outfile="",type="FORK")


		res=parSapply(cl,datalist,function(eij,yrs){sapply(yrs, diffDataYear,d=pattern,e=eij,prop=prop,diff=diff,type=type)},yrs=years)
		stopCluster(cl)
	}
	else
		res=parSapply(datalist,function(eij,yrs){sapply(yrs, diffDataYear,d=pattern,e=eij,prop=prop,diff=diff)},yrs=years)
	return(res)
}

