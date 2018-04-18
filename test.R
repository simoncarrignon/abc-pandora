library(microbenchmark)
library(parallel)
 #simsample=read.csv("realIntro/CITIESPL/eps_0.0894/1001_7_0.912010943817_9.1299684722_6.29840023737/agents.csv",sep=";")
 #save(simsample,file="simsample.bin")
 load("simsample.bin")
##this file used to benchmark two version of the count of number of agent

agentWithMatrixWay <- function(expe,goods=NULL,timestep=NULL,breaks=NULL,joinfunction=sum,min=1,numsite=NULL,bias=NULL){

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
	res=matrix(0,nrow=length(timestep),ncol=length(goods))
	rownames(res)=timestep
	colnames(res)=goods

	for( tmstp in timestep){
		cur=expe[expe$timeStep == tmstp,] 
		if(!is.null(bias)){
			tenpercent=(bias*numsite[tmstp])
			ranks=cur[cur$timeStep == unique(cur$timeStep)[1],c("agent","size")] #the rank change through time because i implemanted it like that thus I have to recompute the ranks each time. That sucks, and maybe I should force the size of the new consumer to always be low, and here don't have to worry anymore
			topten=ranks$agent[order(ranks$size,decreasing = T)][1:tenpercent] #when biased toward the big cities we sample the cite but by take at least the 10% of the biggest cities
			rest=cur$agent[!(cur$agent %in% topten )] #the agents from which we will randomly select
			random=unique(rest)[sample.int(length(unique(rest)))] 
			selectedAG=unlist(list(topten,random))

		}
		else{
			##random sampling amoung the agent
			selectedAG=levels(cur$agent)[sample.int(length(levels(cur$agent)))]
		}
		selectedAG=sort(selectedAG)
		subsample=cur[which(cur$agent %in% selectedAG),]
		#subsample=droplevels(cur)
		for(g in goods){

			cur_res=0;
			if(!is.null(breaks)){#if we want to breaks the dataset in period then we need to put join the timestep of a same period using joinfunction (usually 'sum') 
				join=tapply(subsample[,paste(g,"_q",sep="")],subsample$agent,joinfunction)
				if(min==0)cur_res=(length(join[join>=min]))
				if(min==1)cur_res=(length(join[join>min]))
			}
			else{ #if not we just count the number of agent with a quantity > the min
				if(min==1)cur_res=(length(subsample[ subsample[,paste(g,"_q",sep="")] >= min,"agent"]))
				if(min==0)cur_res=(length(subsample[ subsample[,paste(g,"_q",sep="")] > min,"agent"]))
			}
			res[as.character(tmstp),g]=cur_res
		}

	}

	return(res)
}


forl=microbenchmark(agentWithMatrixWay(mi,numsite=250,breaks=50))
sapl=microbenchmark(agentWith(mi,numsite=250,breaks=50))

##result show no difference in time execution, though slight diffference of the counts


 sapply(seq(10,80,20),function(i)getRealDataCount(i,proportion=F)-realdatadiversities[[as.character(i)]])
 sapply(seq(10,80,20),function(i)getRealDataCount(i,proportion=T)-getprop(realdatadiversities[[as.character(i)]]))
 sapply(seq(10,80,20),function(i)getRealDataCount(i,proportion=T,pattern="dis")-getprop(realdatadistributions[[as.character(i)]]))
 sapply(seq(10,80,20),function(i)getRealDataCount(i,proportion=F,pattern="dis")-realdatadistributions[[as.character(i)]])

 ###number of site should always be the same
 sum(getRealDataCount(10))
 apply(getRealDataCount(10,prop=F),1,sum)
 apply(getRealDataCount(200,prop=F),1,sum)
 apply(getRealDataCount(20,prop=F,goods=c("ESA","ESB","ESC")),1,sum)
 apply(getRealDataCount(200,prop=F,goods=c("ESA","ESB","ESC")),1,sum)
 ####

 sum(getRealDataCount(10))
 apply(getRealDataCount(10,"dis",prop=F),2,sum)
 apply(getRealDataCount(200,"dis",prop=F),2,sum)
 apply(getRealDataCount(20,"dis",prop=F,goods=c("ESA","ESB","ESC")),2,sum)
 apply(getRealDataCount(200,"dis",prop=F,goods=c("ESA","ESB","ESC")),2,sum)

 goods=c("ESA","ESB","ESC","ESD","ITS")
 for(i in 1:length(goods)){
	 print(i )
	 #print(getRealDataCount(numperiods=20,goods=goods[1:i],proportion=F,pattern="dis"))
	 print(agentWith(simsample,numperiods=20,goods=goods[1:i],proportion=F,pattern="dis"))

 }
 for(i in 1:length(goods)){
	 print(i )
	 print(getRealDataCount(numperiods=20,goods=goods[sample.int(length(goods),i)],proportion=T,pattern="dis"))
	 print(agentWith(simsample,numperiods=20,goods=goods[sample.int(length(goods),i)],proportion=T,pattern="dis"))
 }
 for(i in 1:length(goods)){
	 print(i )
	 print(getRealDataCount(numperiods=20,goods=goods[sample.int(length(goods),i)],proportion=T,pattern="div"))
	 print(agentWith(simsample,numperiods=20,goods=goods[sample.int(length(goods),i)],proportion=T,pattern="div"))
 }

 for(i in 2:length(goods)){
	 print(i )
	 real=getRealDataCount(numperiods=20,goods=goods[sample.int(length(goods),i)],proportion=T,pattern="div")
	 simu=agentWith(simsample,numperiods=20,goods=goods[sample.int(length(goods),i)],proportion=T,pattern="div")
	 print( zscore(real,simu))
 }

 for(i in seq(155,400,50)){
	 for(pr in c(T,F)){
		 for(pa in c("div","dis")){
			 print(paste(i,pr,pa))
			 real=getRealDataCount(numperiods=i,proportion=pr,pattern=pa)
			 simu=agentWith(simsample,numperiods=i,proportion=pr,pattern=pa)
			 print( zscore(real,simu))
		 }
	 }
 }
 #number of site testing
 for(i in seq(2,length(unique(simsample$timeStep)),50)){
	 for(pr in c(T,F)){
		 for(pa in c("div","dis")){
			 print(paste(i,pr,pa))
			 real=getRealDataCount(numperiods=i,proportion=pr,pattern=pa)
			 ns=round(runif(1,1,200))
			 print(paste("number of site",ns))
			 simu=agentWith(simsample,numperiods=i,numsite=ns,proportion=pr,pattern=pa,bias=1)
			 print( zscore(real,simu))
		 }
	 }
 }

 ###BIAS testing
 for(i in 2:length(goods)){
	 print(i )
	 real=getRealDataCount(numperiods=20,goods=goods[sample.int(length(goods),i)],proportion=T,pattern="div")
	 ns=round(runif(1,1,200))
	 bs=round(runif(1,0,1))
	 print(paste("number of site",ns))
	 simu=agentWith(simsample,numperiods=20,goods=goods[sample.int(length(goods),i)],proportion=T,pattern="div")
	 print( zscore(real,simu))
 }

	origin=unique(simsample$agent)
	origin=unique(simsample$agent)
	ranks=unique(simsample[simsample$timeStep == unique(simsample$timeStep)[1],c("agent","size")]) 

 for(ss in seq(1,10,5)){
 for(bi in seq(0,1,0.2)){
	 print(paste(ss,bi))
	 ssmpl=getSample(origin,ranks,bi,ss)
	 print(paste(length(ssmpl),length(unique(ssmpl)),ss,length(origin)))
	 print(sort(ssmpl))

 }
 }

 for(ss in seq(2,10,5)){
 for(bi in seq(0,1,0.2)){
	 for(pr in c(T,F)){
		 for(pa in c("div","dis")){
			 print(paste(i,pr,pa))
			 real=getRealDataCount(numperiods=20,proportion=pr,pattern=pa)
			 simu=agentWith(simsample,numperiods=20,numsite=ss,proportion=pr,pattern=pa,bias=bi)
			 print( zscore(real,simu))
		 }
	 }

 }
 }

 for(ss in seq(2,20,5)){
 for(bi in seq(0,1,0.2)){
	 for(pr in c(T,F)){
		 for(pa in c("div","dis")){
			 print(paste(ss,bi))
			 real=getRealDataCount(numperiods=20,proportion=pr,pattern=pa)
			 simu=agentWith(simsample,numperiods=20,numsite=ss,proportion=pr,pattern=pa,bias=bi)
			 print( zscore(real,simu))
		 }
	 }

 }
 }


 parSapply(cl,seq(10,100,5),function(ss)agentWith(simsample,numperiods=ss,numsite=40,proportion=F,pattern="div",bias=1))
 ulu=sapply(seq(10,100,5),function(ss)zscore(agentWith(simsample,numperiods=ss,numsite=40,proportion=F,pattern="div",bias=1),getRealDataCount(numperiods=ss,pattern="div",proportion=F)))
 
		
	 for(pr in c(T,F)){
		 for(pa in c("div","dis")){
			 print(paste(ss,bi))
			 print(agentWith(simsample,numperiods=3,numsite=2,proportion=pr,pattern=pa,bias=1))
		 }
	 }

			 agentWith(simsample,numperiods=3,numsite=100,proportion=pr,pattern="dis",bias=.5,min=1)
	 agentWith(simsample,numperiods=10,numsite=6000,proportion=pr,pattern=pa,bias=.1,min=1)
			 agentWith(simsample,numperiods=3,numsite=1,proportion=pr,pattern=pa,bias=1,min=1)

 bias=seq(0,1,.2)
 names(bias)=seq(0,1,.2)
microbenchmark( sapply(bias, function(b)sapply(1:10,function(c)zscore(real,agentWith(simsample,numperiods=20,numsite=50,proportion=pr,pattern=pa,bias=b)))),times=1)
  cl <- makeCluster(detectCores(),outfile="",type="FORK") 
for(pr in c(T,F)){
	for(pa in c("div","dis")){
		map=parSapply(cl,bias, function(b)sapply(1:10,function(c)zscore(real,agentWith(simsample,numperiods=20,numsite=50,proportion=pr,pattern=pa,bias=b))))
		boxplot(map,main=paste(pr,pa),ylim=c(0.1,0.15))
	}
}

 score=lapply(list(absdiff=absdiff,zscore=zscore),function(diff)lapply(list(nonprop=F,prop=T),function(p)lapply(list(dis=realdatadistributions,div=realdatadiversities),function(rdt)getAllScores(smaller[2:6],years,diff=absdiff,pattern=rdt,prop=T))))

years=seq(10,100,30)
names(years)=years
  score=lapply(list(absdiff=absdiff,zscore=zscore),function(diff)
	       lapply(list(nonprop=F,prop=T),function(p)
		      lapply(list(dis="dis",div="div"),function(rdt)
			     getAllScores(smaller[2:5],allperiods=years,diff=diff,pattern=rdt,proportion=p)
			     )
		      )
	       )


getAllScores(smaller[2:5],allperiods=years,diff=absdiff,pattern="dis",proportion=T,par=F)
getAllScores(smaller[2:5],allperiods=years,diff=enriscore,pattern="dis",proportion=T,par=F)

#plotSiteWithGood( getRealDataCount(400,proportion=F))
#dev.new()
#plotSiteWithGood( getRealDataCount(200,proportion=F))
#dev.new()
#plotSiteWithGood( getRealDataCount(100,proportion=F))

countest=agentWith(simsample,numperiods=20,pattern="dis",numsite=40,proportion=F)
rco=getRealDataCount(numperiods=20,pattern="dis",proportion=F)
enriscore(countest,countest)
absdiff(countest,countest)
zscore(countest,countest)
difzs(countest,rco)


corrected_zscore=t(apply(absscore,1,function(x)if(x>0){(x-corrected_mean)/corrected_sd}else rep(0,length(x))))
corrected_zscore=t(apply(absscore,1,function(x) print(x[x>0])))

apply(absscore,1,function(x)print(x[x>0]))
corrected_mean


car(mfrow=c(3,2))
plotSiteWithGood(countest) 
plotSiteWithGood(rco)
plotSiteWithGood(absscore) 
plotSiteWithGood(abs(colzscore))
plotSiteWithGood(absscore^2)
plot(apply(abs(colzscore),1,mean),type="l",lwd=3)
zscore(countest,rco)



czf(rco,rco+(runif(length(countest))*50))
enriscore(rco,rco+(runif(length(countest))*10))
enriscore(rco,rco+(runif(length(countest))*10))
czf(rco,rco+(runif(length(countest))*50))
scorefun=list(enriscore=enriscore,zscore=zscore,absdiff=absdiff,czf=czf,simplediff=simplediff)
names(scorefun)=list(enriscore,zscore,absdiff,czf,simplediff)
tut=sapply(scorefun,function(diff,data=rco)sapply(exp(seq(.5,5,.1)),function(noise)diff(data,data+(runif(length(data))*noise))))

testMultisore <- function(){
printbest(enscore,"enriscore", "prop","dis")
dafuk$absdiff$prop$both[paste0(getbest(enscore$absdiff$prop$dis),".50")]-(1/2*enscore$absdiff$prop$dis[paste0(getbest(enscore$absdiff$prop$dis),".50")]+1/2*enscore$absdiff$prop$div[paste0(getbest(enscore$absdiff$prop$dis),".50")])
}

	microbenchmark(sapply(datalist[1],function(eij)sapply(years,function(y){zscore(t(agentWith(eij$data,breaks=y,numsite=40,type="count")),realdatadistributions[[as.character(y)]])})),time=1)
