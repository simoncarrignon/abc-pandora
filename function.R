simpsonDiv <- function(x)sum((x/sum(x))^2) #Compute the simpson diversity index as 

##return proportions 
getprop  <- function(x)x/apply(x,1,sum)

simpscore <- function(sim,dat) mean(abs(apply(sim,1,simpsonDiv)-apply(dat,1,simpsonDiv)))

zscore <- function(sim,dat){abs(mean(apply((abs(sim-dat)-apply(abs(sim-dat),2,mean))/apply(abs(sim-dat),2,sd),2,mean)))}

difzs <- function(sim,dat){return(realzscore(sim)-realzscore(dat))}

realzscore <- function(dat){
	mu=apply(dat,2,mean)
	si=apply(dat,2,sd)
	(dat-mu)/si
	abs(t(apply(dat,1,function(x)(x-mu)/si)))
}

absdiff <- function(sim,dat){mean(apply(abs(sim-dat),2,mean))}

enriscore <- function(sim,dat){sqrt(sum((sim-dat)^2))/length(sim)}

czf <- function(sim,dat){
	absdif=abs(sim - dat)
	corrected_mean=apply(absdif,2,function(c)mean(c[c>0]))
	corrected_sd=apply(absdif,2,function(c)sd(c[c>0]))
	colzscore=sapply(colnames(absdif),function(col){absdif[absdif[,col]>0,col]=(absdif[absdif[,col]>0,col] - corrected_mean[col])/corrected_sd[col];return(absdif[,col])})
	return(mean(apply(abs(colzscore),1,mean)))
}
simplediff <- function(sim,dat){mean(abs(sim - dat))}


computeSimpsonForOneExpe  <-  function(expe,jf=sum,breaks,min)apply(agentWith(expe,numperiods=breaks,joinfunction=jf,min=min),1,simpsonDiv) #this compute  the  simson index of the number of settlement with differents good for one experiments


#from a given ector of name of size N, return a vector of size N2 size(N2)<size(N)
getSample  <- function(origin,ranks,bias,subsize){
	origin=as.character(origin)
	subsample=c()

	nfixedsites=round((bias*min(subsize,length(origin)))) #number of site selected for their size
	nrandomsites=round(min(subsize,length(origin))-nfixedsites)  #number of site randomly selected

	fixed=as.character(ranks$agent[order(ranks$size,decreasing = T)][1:nfixedsites] )#when biased toward the big cities we sample the cite but by take at least the 10% of the biggest cities
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
#numperiods = the number of periods used 
#numsite = if we are NOT using all the sites, one can: determine a number of site or give vector of number of site that will be used for each timestep
#bias, if using a number of site < of the total number of agents then this allow to fix a percentage of the bigger sites that will be allways used
agentWith <- function(expe,goods=NULL,timestep=NULL,numperiods=NULL,joinfunction=sum,min=1,numsite=NULL,bias=1,pattern="div",proportion=T){
	if(is.null(goods))
		goods=levels(expe$p_good)[which(levels(expe$p_good) != "coins")]
	if(!is.null(numperiods))
		if( numperiods > length(unique(expe$timeStep)))
			stop(sprintf("length of the simulation doesn't allow to cut in %d periods",numperiods))
		else
			expe$timeStep=cut(expe$timeStep,breaks=numperiods,label=F)
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

	finalres=sapply( timestep, function(tmstp){
	       tmstp=as.character(tmstp)
	       cur=cur[cur$timeStep == tmstp,] 

	       origin=droplevels(unique(cur$agent))
	       ranks=unique(cur[cur$timeStep == unique(cur$timeStep)[1],c("agent","size")]) #the rank change through time because i implemanted it like that thus I have to recompute the ranks each time. That sucks, and maybe I should force the size of the new consumer to always be low, and here don't have to worry anymore
	       ranks=ranks[ranks$agent %in% origin,]
	       selectedAG=getSample(origin,ranks,bias,numsite[tmstp])
	       cur=cur[cur$agent %in% selectedAG,]
	       cur=droplevels(cur)

	       if(pattern=="div"){
		       counpattern = sapply(selectedAG,function(ag){
					    if(min==0)sum(apply(cur[cur$agent == ag,paste0(goods,"_q") ] ,2,sum)>0)
					    if(min==1)sum(apply(cur[cur$agent == ag,paste0(goods,"_q") ] ,2,sum)>=min)
			})
		       res=table(factor(counpattern,levels=0:length(goods)))
		       return(res)

	       }
	       if(pattern=="dis"){
		       sapply(goods,function(g){

			      if(!is.null(numperiods)){#if we want to numperiods the dataset in period then we need to put join the timestep of a same period using joinfunction (usually 'sum') 
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
	finalres = t(finalres)
	if(proportion){
		finalres=getprop(finalres)
		finalres[is.na(finalres)]=0
	}
	return(finalres)
}


#agentWithwrapper
getSimuCount <- function(numperiods,pattern="div",goods=NULL,proportion=T){
	return(agentWith(expe,goods=NULL,timestep=NULL,numperiods=NULL,joinfunction=sum,min=1,numsite=NULL,bias=1,pattern="div"))
}



diffData <- function(numperiods=40,simu,proportion=T,diff=absdiff,pattern="div",numsite=40){

    diffstr=deparse(substitute(diff))

    #TODO : read only if not already in global var and better handle the errors boyyzz
    countid=paste(gsub("/","",simu))
    if(!exists(countid)){
        print(countid)
        dataexp=read.csv(file.path(simu,"agents.csv"),sep=";")
        assign(countid,dataexp,envir=.GlobalEnv)
    }
    else
        dataexp=get(countid)

    print(paste("compute:",numperiods,proportion,pattern,simu))
    dt=getRealDataCount(numperiods=numperiods,proportion=proportion,pattern=pattern)
    tryCatch(
             {
                 rdt=agentWith(dataexp,numperiods=numperiods,proportion=proportion,pattern=pattern,bias=1,numsite=numsite)
                 return(diff(dt,rdt))
             },error=function(err){return(NA)})
}


#datalist=list with all data
#years list of years bininb
#diff function to comput distance
#pattern realdata to ompare 
#par=T true if to be parallelized
#prop=T true if to use proportions
getAllScores <- function(datalist,allperiods,diff,pattern,par=T,proportion=T,numsite=40){
	print(par)
	print(length(datalist))


	if(par==T){
		cl <- makeCluster(detectCores()-1,outfile="",type="FORK")


		res=parSapply(cl,datalist,function(eij,prd){sapply(prd, diffData,simu=eij,proportion=proportion,diff=diff,pattern=pattern,numsite=numsite)},prd=allperiods)
		stopCluster(cl)
	}
	if(par=="mpi")
		res=mpi.parSapply(datalist,function(eij,prd){sapply(prd, diffData,simu=eij,proportion=proportion,diff=diff,pattern=pattern,numsite=numsite)},prd=allperiods)
	else
		res=sapply(datalist,function(eij,prd){sapply(prd, diffData,simu=eij,proportion=proportion,diff=diff,pattern=pattern,numsite=numsite)},prd=allperiods)
	return(res)
}


getRealDataCount <- function(numperiods,pattern="div",goods=NULL,proportion=T){


	#check if the simpson diversity for thoses steps has been already computed
	#this file is stored just one folder before which means that we expect the experiemnt to be stored in one folder under the Folder where they are launched. 
	#Which is true given the implementation of the classe Experiment in ceec.py (generateState line 103 & initialisation line 54)
	#This script as no meaning outside the ABC framework
	#In fact this script SHOULD BE implemented as a method eof Experiments, in order to allow the framework to work with any kind of EXPERIMENTS
	if(is.null(goods))
		goods=c("ESA","ESB","ESC","ESD","ITS")

	filenameBackup=paste0("realcount-",numperiods,"-",pattern,"-prop",proportion,"-",concatlast(goods),".bin")
	if(!file.exists(filenameBackup)){ ##if else to avoid recreate each time very long file
		realdata=generateDataCount(numperiods,pattern,proportion,goods)
		save(realdata,file=filenameBackup)
	}else{
		load(filenameBackup)
	}
	return(realdata)

}


#generateDataCount <- function(numperiods,pattern="div",proportion=T,goods=NULL){
#this is the pending version of agentWith but for the realdata. It should have the same argument and return the same thing
generateDataCount <- function(numperiods,pattern="div",proportion=T,goods=NULL){
	if(!(pattern %in% c("dis","div"))){
		print("unrecognized pattern")
		stop(-1)
	}
	data=read.csv("~/data_per_year.csv")
	data$goods=data$Fabric

	if(is.null(goods))
		goods=levels(data$goods)
	names(goods)=goods
	data$date=cut(data$date,breaks=numperiods) 
	if(pattern=="dis")
		realdata=(sapply( goods , function(g)sapply(sort(unique(data$date)),function(ts)length(unique(data$Location_ascii[data$date == ts & data$goods == g])))) )
	if(pattern=="div")
		realdata=(t(sapply( levels(data$date) , function(ts) table(factor(sapply(unique(data$Location_ascii),function(ag)length(unique((data$goods[data$Location_ascii==ag & data$date == ts & data$goods %in% goods])))) ,levels=0:length(goods))))))
	if(proportion)return(getprop(realdata))
	else return(realdata)
}

##return a string made of all the las letter of all the strings in strcev
concatlast <- function(strvec){
	strvec=sort(as.character(strvec))
	paste0(substr(strvec,nchar(strvec),nchar(strvec)),collapse="")   
}

generetclust <- function(data,numperiods=100){

	library(vegan)
	library(ape)
	data$date=cut(data$date,breaks=numperiods) 

	u=tapply(data$X, data[,c("date","Fabric_rough","Location_ascii")],length)
	u[is.na(u)]=0  
	lisdist=lapply(rownames(u),function(l)vegdist(t(u[l,,]),method="jaccard",na.rm=T))
	lisdist=lapply(rownames(u),function(l)vegdist(t(u[l,,]),method="jaccard",na.rm=T))
	names(lisdist)=rownames(u)
	names(lisdist)
	lisdist=lapply(lisdist,function(i){i[is.na(i)]=0;return(i)})
	njlist=lapply(lisdist,function(i){nj(i,"unrooted")})
plot(njlist[[1]])

	sapply(seq(1,500,10),function(r){png(sprintf("Bphyltree%03d.png",r),width=1400,height=1400);plot(root(as.phylo(hclust(lisdist[[names(lisdist)[r]]])),"athens"),main=paste("year",rownames(u)[r]));dev.off()})

	sapply(seq(1,500,10),function(r){png(sprintf("tree%03d.png",r),width=1400);plot(as.dendrogram(hclust(dist(t(u[r,,])))),main=paste("year",rownames(u)[r]));dev.off()})
	sapply(seq(1,500,10),function(r){png(sprintf("unroottree%03d.png",r),width=1400,height=1400);plot(as.phylo(hclust(dist(t(u[r,,])))),main=paste("year",rownames(u)[r]),type="unrooted");dev.off()})
	sapply(seq(1,500,10),function(r){png(sprintf("fantree%03d.png",r),width=1400,height=1400);plot(as.phylo(hclust(dist(t(u[r,,])))),main=paste("year",rownames(u)[r]),type="fan");dev.off()})
	sapply(seq(1,500,10),function(r){png(sprintf("phyltree%03d.png",r),width=1400,height=1400);plot(as.phylo(hclust(dist(t(u[r,,])))),main=paste("year",rownames(u)[r]));dev.off()})
	sapply(seq(1,500,10),function(r){png(sprintf("phyltree%03d.png",r),width=1400,height=1400);plot(root(as.phylo(hclust(dist(t(u[r,,])))),"athens"),main=paste("year",rownames(u)[r]));dev.off()})
	sapply(seq(1,500,10),function(r){png(sprintf("jac_phyltree%03d.png",r),width=1400,height=1400);plot(root(as.phylo(hclust(lisdist[[r]])),"athens"),main=paste("year",rownames(u)[r]));dev.off()})
}

jaccard <- function(a,b){
	m11=sum( a == 1 & b ==1)
	m01=sum( a == 0 & b ==1)
	m10=sum( a == 1 & b ==0)
	print(m11)
	print(m01)
	print(m10)
	return(m11/(m01+m10+m11))
}
