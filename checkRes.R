source("function.R")
source("abctools.R")


#file copied from previous script to draw the results of the ABC runs

getlistParticlesFromPath <- function(path){
	lf=list.files(path,pattern="resul_*")
	epsilon=sort(sub("result_(.*).csv","\\1",lf),decreasing=T)
	listParticles=lapply(epsilon,function(eps){print(eps);cbind(read.csv(paste(path,"result_",eps,".csv",sep="") ),epsilon=eps)})
	names(listParticles)=epsilon
	listParticles=lapply(listParticles,function(u) {u$ratio = (u$cstep)/u$nstep; return(u)})
	return(listParticles)
}

computeScores  <-  function(fold,sample,...){

	expe=list()
	expe[["folder"]]=fold
	expe[["data"]]=read.csv(file.path(fold,"agents.csv"),sep=";")
	expe[["counted"]]=agentWith(expe[["data"]],breaks=nrow(realdata))
	expe[["simpscore"]]=simpscore(expe[["counted"]],realdata)
	expe[["sampled_count"]]=lapply(1:sample,function(i){print(i);t(agentWith(expe[["data"]],breaks=nrow(realdata),...))})
	expe[["zscores"]]=sapply(expe[["sampled_count"]],zscore,realdata)
	return(expe)

}

computeScoresDat  <-  function(dat,sample,realdata,...){

	expe=list()
	expe[["sampled_count"]]=lapply(1:sample,function(i){print(i);t(agentWith(dat,breaks=nrow(realdata),...))})
	expe[["zscores"]]=sapply(expe[["sampled_count"]],zscore,realdata)
	return(expe)

}


plotZscoreAndSimp <- function(dat,ranked=T,...){
	if(ranked)ranks=order(unlist(lapply(dat["zscores",],mean)))
	else ranks=1:length(dat["zscores",])
	print(ranks)
	#plot(1:ncol(dat),dat["simpscore",ranks],col="red",pch=20,cex=2,axes=F,xlab="",ylim=c(0.1-0.015,0.155+0.015))
	plot(1:ncol(dat),dat["simpscore",ranks],col="red",pch=20,cex=2,axes=F,xlab="",ylab="")#,ylim=c(0.1-0.015,0.155+0.015))
	axis(4,col="red",lwd=2,col.axis="red")
	par(new=T)
	plot(c(1,ncol(dat)),range(sapply(dat["zscores",],range)),type="n",ylab="zscores",xlab="rank (given by mean zscore)",...)
	sapply(1:ncol(dat),function(i)try(vioplot(dat[["zscores",ranks[i]]],col=NA,at=i,add=T)))
}


main <- function(){
	res=read.csv("result_4.5.csv")
	par(mfrow=c(3,1))
	hist(res$mu,breaks=50)
	hist(res$sd,breaks=50)
	hist(res$n,breaks=50)
	dev.new()

	res=read.csv("result_4.5.csv")
	par(mfrow=c(3,1))
	hist(res$mu,breaks=50)
	hist(res$sd,breaks=50)
	hist(res$n,breaks=50)


epsilon=c(5,4.75,4.5)


    uuu2=read.csv("~/share_res/result_1.00.csv")
    uuu=read.csv("~/share_res/luv/")
    uuu=read.csv("~/share_res/result_0.20.csv")

    path="~/share_res/luv/"
    epsilon=c("0.30","0.20","0.10")
    uuu2=read.csv("~/share_res/full/result_0.50.csv",sep= ";", header=F)
    uuu1=read.csv("~/share_res/full/result_0.48.csv",sep= ";", header=F)
    uuu0=read.csv("~/share_res/full/result_0.46.csv",sep= ";", header=F)
    uuu4=read.csv("~/share_res/full/result_0.44.csv",sep= ";", header=F)

    path="~/share_res/testAg/"
    plotDensities(path,"mu",c("2000.00","0.20"))

    plotDensities(path,"mu",epsilon)

    path="~/share_res/noNegat//"
    plotDensities(path,"mu",c("2000.00","0.20"))

    pathBig="~/share_res/biggerThanPhat/"
    plotDensities(pathBig,"cstep",c("3000.00","0.20"))
    tt=read.csv("~/share_res/testMu/3_100_0.6_20_0.124471348764/agents.csv",sep=";")
    tt2=read.csv("~/share_res/testMu/3_100_0.6_20_0.832957020173/agents.csv",sep=";")

    alltt=rbind(tt,tt2)

    alltt=alltt[alltt$timeStep >= 6000,]

    tt=read.csv("~/share_res/biggerThanPhat/2_199_0.508440976643_33_0.967006910121/agents.csv",sep=";")
    tt2=read.csv("~/share_res/biggerThanPhat/3_387_0.827853613564_24_0.214588177227/agents.csv",sep=";")

    mean(tt$score[tt$timeStep == max(tt$timeStep)])/(33*1)
    mean(tt2$score[tt2$timeStep == max(tt2$timeStep)])/(24*2)

    alltt=rbind(tt,tt2)

    boxplot(alltt$score~ alltt$mu)
    getPropFromXml("~/share_res/testMu/3_100_0.6_20_0.124471348764/","controller","step")
    getPropFromXml("~/share_res//3_100_0.6_20_0.124471348764/","controller","step")

    path="~/share_res/smallTight/"
    plotDensities(path,"cstep",c("3000.00","0.05"))

    path="~/share_res/testMuBisWider/"
    path="~/share_res/testMutRand200/"
    tt2=read.csv("~/share_res/testMuBisWider/3_50_0.6_10_0.000590196537098/agents.csv",sep=";")
    tt2=getAllMeanScore("~/share_res/testMu/")
    pposteriorlotDensities(path,"mu",c("3000.00","0.25"))
    #Devrait pouvoir etre assz utile de faire une fonction qui genere cce graphe de façon auto en fonction de deux variable
    plot(density(uuu$V3),xlim=range(c(density(uuu2$V3)$x,density(uuu1$V3)$x,density(uuu0$V3)$x)),ylim=range(c(density(uuu2$V3)$y,density(uuu1$V3)$y,density(uuu0$V3)$y)),)

    lines(density(uuu1$V1),col="orange")
    lines(density(uuu0$V3),col="red")

    #pdf("../../../doc/conferences/20170127_YSLRWinterWorkshop/img/ABC.pdf")
    pdf("~/resultsABC.pdf")
    par(mar=c(5,4,1,1))
     plotDensities(path,"mu",c("3000.00","0.90"),from=0,to=1,xlim=c(0,1))

    dev.off()
    par(mfrow=c(2,2))
    epsilon=c("0.25","0.2036")
     plotDensities(path,"mu",epsilon,0,1)
     plotDensities(path,"mumax",epsilon,0.5,15)
     plotDensities(path,"copy",epsilon,0,10)
     plotDensities(path,"cstep",epsilon,1,8)
     plotDensities(path,"cstep",epsilon,1,8)
     #plotDensities(path,"n",epsilon,40,70)
     plot(ll[["0.2036"]]$nstep  ~ ll[["0.2036"]]$cstep)
     a=ll[["0.2036"]]$nstep/ll[["0.2036"]]$cstep
    plot(density(a))
     plotDensities(llbis,"frac",epsilon,0,0.04)
}

exploreDensities <- function(){
path="./"
epsilon=c("1000.0","0.25","0.2036","0.1658","0.1351")
ll=plotDensitiesFrompath(path,"nstep",epsilon,150,360)
llbis=lapply(ll,function(u) {u$ratio = (u$cstep)/u$nstep; return(u)})

pdf("testaccio.pdf")
plotAllDensities(llbis)
dev.off()

path="./new_abc/"
epsilon=c("1000.0","0.25","0.2036","0.1658")
ll=plotDensitiesFrompath(path,"nstep",epsilon,150,360)
llbis=lapply(ll,function(u) {u$ratio = (u$cstep)/u$nstep; return(u)})

pdf("testaccioNew.pdf")
plotAllDensities(llbis)
dev.off()


scoreWithoutSample=read.csv("scoresTis.csv",header=F)
allres=sapply(paste0(scoreWithoutSample$V2,"agents.csv"),read.csv,sep=";" )
small=read.csv(paste0(scoreWithoutSample$V2[scoreWithoutSample$V1 == min(scoreWithoutSample$V1)],"agents.csv"),sep=";" )
big=read.csv(paste0(scoreWithoutSample$V2[scoreWithoutSample$V1 == max(scoreWithoutSample$V1)],"agents.csv"),sep=";" )
scoreWoSample=list(max=big,min=small)

scoreWithSample=read.csv("scoresTis.csv",header=F)
allres=sapply(paste0(scoreWithSample$V2,"agents.csv"),read.csv,sep=";" )
small=read.csv(paste0(scoreWithSample$V2[scoreWithSample$V1 == min(scoreWithSample$V1)],"agents.csv"),sep=";" )
big=read.csv(paste0(scoreWithSample$V2[scoreWithSample$V1 == max(scoreWithSample$V1)],"agents.csv"),sep=";" )
scoreWSample=list(max=big,min=small)

pdf("scoreTakingAllCities.pdf",width=3)
par(mfrow=c(3,1))
plotSiteWithGood(agentWith(scoreWoSample[["min"]]))
plotSiteWithGood(agentWith(scoreWoSample[["max"]]))
plotSiteWithGood(realdata)
dev.off()

dev.new()
pdf("scoreSelectingSubsampleOfCities.pdf",width=3)
par(mfrow=c(3,1))
plotSiteWithGood(agentWith(scoreWSample[["min"]]))
plotSiteWithGood(agentWith(scoreWSample[["max"]]))
plotSiteWithGood(realdata)
dev.off()

dev.new()
pdf("scoreSelectingBiasedSubsampleOfCities.pdf",width=3)
par(mfrow=c(3,1))
plotSiteWithGood(agentWith(scoreWSample[["min"]],numsite=150,breaks=nrow(realdata),bias=.1))
plotSiteWithGood(agentWith(scoreWSample[["max"]],numsite=150,breaks=nrow(realdata),bias=.1))
plotSiteWithGood(realdata)
dev.off()

dev.new()
pdf("scoreSelectingSubsampleOfCitiesMapData.pdf",width=3)
par(mfrow=c(3,1))
plotSiteWithGood(agentWith(scoreWSample[["min"]],numsite=nsiteData,breaks=nrow(realdata)))
plotSiteWithGood(agentWith(scoreWSample[["max"]],numsite=nsiteData,breaks=nrow(realdata)))
plotSiteWithGood(realdata)
dev.off()

plotSiteWithGood( agentWithSampled(small,origin=data))
par(mfrow=c(1,1))
allSd=sapply(allbias,function(u)sapply(u[["scores"]]["zscores",],sd))
}

exploreZscore <- function(){
	allscores=sapply(list.dirs("~/share_res/testScoreSerialx/",recursive=F)[1:20],function(i){print(i);return(computeScores(i,sample=100,numsite=200))})
	allscoresNB=sapply(list.dirs("~/share_res/testScoreSerialx/",recursive=F)[1:2],function(i){print(i);return(computeScores(i,sample=100,numsite=200,bias=.1))})

	par(mfrow=c(1,2))
	plotZscoreAndSimp(allscores,xlim=c(0,3),ylim=c(0.2,0.5))
	plotZscoreAndSimp(allscoresNB,xlim=c(0,3),ylim=c(0.2,0.5))

	biasinogrel46
	alldirs=c(list.dirs("realIntro/realintrSmallEpsBADPOWERBADSCORE/eps_0.0562",recursive=F),list.dirs("realIntro/realintrSmallEpsBADPOWERBADSCORE/eps_1000.0",recursive=F),list.dirs("realIntro/realintrSmallEpsBADPOWERBADSCORE/eps_0.1",recursive=F))
	allbias=list()
	for(fold in alldirs){
		try({
			print(fold)
			allbias[[fold]][["data"]]=read.csv(file.path(fold,"agents.csv"),sep=";")
			allbias[[fold]][["counted"]]=t(agentWith(allbias[[fold]][["data"]],breaks=nrow(realdata)))
			allbias[[fold]][["simpscore"]]=simpscore(allbias[[fold]][["counted"]],realdata)
			allbias[[fold]][["scores"]]= sapply(seq(0,0.95,0.1),function(i){print(i);return(computeScoresDat(allbias[[fold]][["data"]],sample=45,numsite=100,bias=i,realdata=realdata))})
		})
	}

	print(computeScores("~/share_res/testScoreSerialx/exp_1/",sample=1,numsite=250,bias=.2)$zscore)

	biasing=allbias[[1]]
	plotZscoreAndSimp(biasing)
	plotZscoreAndSimp(allscores)

	allscores=allbias[[1]]

	pdf("ZscoreDistribapdf" )
	plotZscoreAndSimp(biasing,xlim=c(0,51))
	dev.off()

	vioplot(allscores["zscores",])
	( simplify2array(lapply(allscores["simpscore",],rep,length(allscores[["zscores",1]])))~simplify2array(allscores["zscores",]) )
	plot( simplify2array(lapply(allscores["simpscore",],rep,length(allscores[["zscores",1]])))~simplify2array(allscores["zscores",]) )
	plot( simplify2array(lapply(allscoresscores["zscores",],sd))~simplify2array(lapply(allscores["zscores",],mean)))

	nsiteData=apply(realdata,1,sum)
	par(mfrow=c(1,1))
	test=allscores[["data",1]]
	plot( test$ESB_q ~ test$timeStep,ylim=c(0,1) )
	plotSiteWithGood(agentWith(allscores[["data",1]],numsite=250,breaks=length(nsiteData)))
	t2=agentWith(allscores[["data",1]])
	plotSiteWithGood(t1)
	pdf("total_num_site.pdf")
	plot(apply(realdata,1,sum),type="l",lwd=4,xlab="time",ylab="total number of sites")
	dev.off()
	ranks=order(unlist(lapply(backscore["zscores",],mean)))
	pdf("rank1.pdf")
	plotSiteWithGood(agentWith(backscore[["data",ranks[1]]]))
	dev.off()
	pdf("rank50.pdf")
	plotSiteWithGood(agentWith(backscore[["data",ranks[50]]]))
	dev.off()
	pdf("real.pdf")
	plotSiteWithGood(realdata)
	dev.off()

	epsilon=c("1000.0","0.2","0.1414","0.1","0.0707","0.05")
	ll=plotDensitiesFrompath("./realIntro/","nstep",epsilon,150,360)
	llbis=lapply(ll,function(u) {u$ratio = (u$cstep)/u$nstep; return(u)})
	epsilon=c("1000.0","0.25","0.2036","0.1658","0.1351")
	ll=plotDensitiesFrompath(path,"nstep",epsilon,150,360)
	llbis=lapply(ll,function(u) {u$ratio = (u$cstep)/u$nstep; return(u)})
	plotAllDensities(llbis)

	mi=read.csv("realIntro/REALINTRO/eps_0.0707/147_8_0.548238607829_5.87507349548_4.04141525762/agents.csv",sep=";")
	mimi=read.csv("realIntro/REALINTRO/eps_0.0707/141_1_0.100955856174_0.132571105273_4.53634146476/agents.csv",sep=";")

	par(mfrow=c(2,1))
	plotSiteWithGood(t(agentWith(mimi,,breaks=30)))
	plotSiteWithGood(realdata)

	plot(1:10,1:10,ylim=c(.0,0.035),xlim=c(0,20),type="n")
	sapply(allbias,function(i)lines(simplify2array(lapply(i["zscores",1:20],sd))))
	sapply(allbias,function(i)lines(simplify2array(lapply(i["zscores",],mean))))
	plot(1:10,1:10,ylim=c(.3,0.9),xlim=c(0,20),type="n")
	sapply(allbias,function(i)lines(simplify2array(lapply(i["zscores",],mean))))

	text(sapply(allbias,function(i)(i[["simpscore"]])),sapply(allbias,function(i)mean(i[["zscores",20]])),labels=sapply(allbias,function(i)(substr(i[["folder",20]],44,55))))
	bias=20
	plot(sapply(allbias,function(i)sd(i[["zscores",bias]])),sapply(allbias,function(i)mean(i[["zscores",bias]])))
	plotSiteWithGood(t(allbias[["/home/scarrign/share_res/testScoreSerialx//exp_19"]][["counted",20]]))

	ll=getlistParticlesFromPath("realIntro/realintrSmallEpsBADPOWERBADSCORE/")
	plotAllDensities(ll)
	plotcov(ll,"cstep","nstep")

	ll=getlistParticlesFromPath("citiespl/")
	plotAllDensities(ll)
	sapply(ll,function(i)i[i$score == min(i$score),])
	plotcov(ll,"mumax","mu")

	par(mfrow=c(1,2))
	plotScoreWrtBias(allbias,fun=mean)
	plotScoreWrtBias(allbias,fun=sd)

	latest=list.dirs("realIntro/CITIESPL/eps_0.2675/",recursive=F)
	alllat=getMaxInfo(latest)

	topteninfos=getMaxInfoSample(names(topten),repeatsampling=2,realdata=allrealdata[["30"]])
	listoffoldsccore=getListScoreFold("realIntro/CITIESPL/")
	topteninfos[[1]][["scores"]]["zscores",]


	topten=sort(aa)[1:20]
	worsteten=sort(aa,decreasing=T)[1:20]

	plot(topten)

	realdatadiversities=list()
	realdatadistributions=list()
	for(year in years){
		print(year)
		realdatadiversities[[as.character(year)]]=generateRealCount(year,type="div")
		realdatadistributions[[as.character(year)]]=generateRealCount(year,type="count")
	}

#	save(realdatadistributions,file="realdatadistributions")
#	save(realdatadiversities,file="realdatadiversities")
#	save(listoffoldsccore,file="listoffoldsccore")
	smaller=datalist[1:500]
	save(smaller,file="smalldatalis")
	#save(datalist,file="datalist")
	load("realdatadistributions")
	load("realdatadiversities")
	load("listoffoldsccore")
	load("multiexp")
	load("zscorePROP")
	load("absdif")
	load("absdifPROP")
	load("smalldatalis")

	#load("datalist")

	absdif=list()
	multiexp=list()

	years=seq(10,100,5)
	names(years)=seq(10,100,5)

	datalist=getDatas(names(listoffoldsccore[sample.int(length(listoffoldsccore),500)]))
	datalist=datalist[sample.int(length(datalist),500,replace=F)]

	print("done datalist===")

	multiexp[["count"]]=sapply(datalist,function(eij){print(".");sapply(years,function(y){tryCatch(zscore(t(agentWith(eij$data,breaks=y,numsite=40,type="count")),realdatadistributions[[as.character(y)]]),error=function(err){NA})})})
	print("done count ===")
	multiexp[["div"]]=	sapply(datalist,function(eij){print(".");sapply(years,function(y){tryCatch(zscore(t(agentWith(eij$data,breaks=y,numsite=40,type="div")),realdatadiversities[[as.character(y)]]),error=function(err){NA})})})
	print("done div ===")


	absdif[["count"]]=sapply(datalist,function(eij){print(".");sapply(years,function(y){tryCatch(absdiff(t(agentWith(eij$data,breaks=y,numsite=40,type="count")),realdatadistributions[[as.character(y)]]),error=function(err){NA})})})
	print("===")
	absdif[["div"]]=sapply(datalist,function(eij){print(".");sapply(years,function(y){tryCatch(absdiff(t(agentWith(eij$data,breaks=y,numsite=40,type="div")),realdatadiversities[[as.character(y)]]),error=function(err){NA})})})

	absdifPROP=list()
	zscorePROP=list()

	zscorePROP[["count"]]=sapply(datalist,function(eij){print(".");sapply(years,function(y){tryCatch(zscore(getprop(t(agentWith(eij$data,breaks=y,numsite=40,type="count"))),getprop(realdatadistributions[[as.character(y)]])),error=function(err){NA})})})
	print("done count ===")
	zscorePROP[["div"]]=	sapply(datalist,function(eij){print(".");sapply(years,function(y){tryCatch(zscore(getprop(t(agentWith(eij$data,breaks=y,numsite=40,type="div"))),getprop(realdatadiversities[[as.character(y)]])),error=function(err){NA})})})
	print("done div ===")



	absdifPROP[["count"]]=sapply(datalist,function(eij){print(".");sapply(years,function(y){tryCatch(absdiff(getprop(t(agentWith(eij$data,breaks=y,numsite=40,type="count"))),getprop(realdatadistributions[[as.character(y)]])),error=function(err){NA})})})
	
	print("===")
	absdif[["div"]]=sapply(datalist[1:10],function(eij){print(".");sapply(years,function(y){tryCatch(absdiff(getprop(t(agentWith(eij$data,breaks=y,numsite=40,type="div"))),getprop(realdatadiversities[[as.character(y)]])),error=function(err){NA})})})


	scoreabsprop$dis=getAllScores(datalist[1:10],years,absdiff,realdatadistributions)
	scoreabs$dis=getAllScores(datalist[1:10],years,absdiff,realdatadistributions,prop=F)
	scorezsc$dis=getAllScores(datalist[1:10],years,zscore,realdatadistributions,prop=F)
	scorezscpro$dis=getAllScores(datalist[1:10],years,zscore,realdatadistributions,prop=T)

	scoreabsprop$div=getAllScores(datalist[1:10],years,absdiff,realdatadiversities)
	scoreabs$div=getAllScores(datalist[1:10],years,absdiff,realdatadiversities,prop=F)
	scorezsc$div=getAllScores(datalist[1:10],years,zscore,realdatadiversities,prop=F)
	scorezscpro$div=getAllScores(datalist[1:10],years,zscore,realdatadiversities,prop=T)
	absdif[["div"]] - scoreabs$div

	score[["zscore"]][["prop"]]$div=getAllScores(datalist[1:10],years,zscore,realdatadiversities,prop=T)

	score[["zscore"]][["prop"]]$dis=getAllScores(datalist[1:10],years,zscore,realdatadistributions,prop=T)

	score[["absdif"]][["nonprop"]]$div=getAllScores(datalist[1:10],years,absdif,realdatadiversities,prop=F)

	score[["absdif"]][["nonprop"]]$dis=getAllScores(datalist[1:10],years,absdif,realdatadistributions,prop=F)

	score[["absdif"]][["prop"]]$div=getAllScores(datalist[1:10],years,absdif,realdatadiversities,prop=T)

	score_prop_absdif=getAllScores(datalist[1:10],years,diff=absdiff,pattern=realdatadistributions,prop=T)
	score_nprop_absdif=getAllScores(datalist[1:10],years,diff=absdiff,pattern=realdatadistributions,prop=F)
	score_nprop_zscore=getAllScores(datalist[1:10],years,diff=zscore,pattern=realdatadistributions,prop=F)

	 score=lapply(list(absdiff=absdiff,zscore=zscore),function(diff)lapply(list(nonprop=F,prop=T),function(p)lapply(list(dis=realdatadistributions,div=realdatadiversities),function(rdt)getAllScores(datalist[1:4],years,diff=diff,pattern=rdt,prop=p))))
test=getAllScores(datalist[1:4],years,diff=absdiff,pattern=realdatadistributions,prop=T)

	#save(zscorePROP,file="zscorePROP" )
	#save(absdifPROP,file="absdifPROP" )
	#save(absdif,file="absdif" )
	#save(multiexp,file="multiexp" )

	years=rownames(eultiexp$count)
	colyear=topo.colors(length(years))
	names(colyear)=years
	plot(multiexp$count ~ multiexp$div,col=colyear[rownames(multiexp$count)],pch=20)
	plot(zscorePROP$count ~ zscorePROP$div,col=colyear[rownames(zscorePROP$count)],pch=20)
	plot(absdifPROP$count ~ absdif$count,col=colyear[rownames(absdifPROP$count)],pch=20)
	lines(multiexp$div,multiexp$count,lwd=.1)
	arrows(multiexp$div[1:(length(multiexp$div)-1)],multiexp$count[1:(length(multiexp$count)-1)],multiexp$div[2:(length(multiexp$div))],multiexp$count[2:(length(multiexp$count))],length=.1,lwd=.8 )
	points(multiexp$count ~ multiexp$div,col=colyear[rownames(multiexp$count)],pch=20)

	legend("topright",legend=years,col=colyear,pch=20,title="#periods")
	plot(absdif$count ~ absdif$div,col=colyear[rownames(absdif$count)],pch=20)
	lines(absdif$div,absdif$count,lwd=.1)
	points(absdif$count ~ absdif$div,col=colyear[rownames(absdif$count)],pch=20)
	legend("bottomright",legend=years,col=colyear,pch=20,title="#periods")

	sapply(datalist,function(u)length(unique(u[["data"]]$timeStep)))

	microbenchmark(sapply(datalist[1],function(eij)sapply(years,function(y){zscore(t(agentWith(eij$data,breaks=y,numsite=40,type="count")),realdatadistributions[[as.character(y)]])})),time=1)

	plot(1,1,ylim=range(multiexp$div,na.rm=T),xlim=c(0,21),ylab="score",xlab="#periods")  
	apply(multiexp$div,2,lines,lwd=.1) 
	plot(1,1,ylim=range(multiexp$count,na.rm=T),xlim=c(0,21))  
	apply(multiexp$count,2,lines,lwd=.1) 

	plot(1,1,ylim=range(absdif$div,na.rm=T),xlim=c(0,12))  
	apply(absdif$div,2,lines) 
	plot(1,1,ylim=range(absdif$count,na.rm=T),xlim=c(0,12))  
	apply(absdif$count,2,lines) 
	names(topten)

	plot(1,1,ylim=range(absdifPROP$div,na.rm=T),xlim=c(0,12))  
	apply(absdifPROP$div,2,lines,lwd=.1) 
	plot(1,1,ylim=range(absdifPROP$count,na.rm=T),xlim=c(0,12))  
	apply(absdifPROP$count,2,lines,lwd=.1) 
	plot(1,1,ylim=range(diszscorePROP$div,na.rm=T),xlim=c(0,12))  
	apply(zscorePROP$div,2,lines,lwd=.1) 
	plot(1,1,ylim=range(zscorePROP$count,na.rm=T),xlim=c(0,12))  
	apply(zscorePROP$count,2,lines,lwd=.1) 
	names(topten)
	names(topten)


}

##From a list of dir,return stuff
getMaxInfoSample <- function(listofdir,repeatsampling=100,numsite=200,realdata){
	result=list()
	
	for(fold in listofdir){
		try({
			result[[fold]]=getFoldExpInfos(fold,repeatsampling,numsite,realdata,nbias=10)
		})

	}
	return(result)
}

getDatas <- function(listofdir){
	result=list()
	
	for(fold in listofdir){
		try({
			result[[fold]][["data"]]=read.csv(file.path(fold,"agents.csv"),sep=";")
		})

	}
	return(result)
}

getFoldExpInfos <- function(fold,repeatsampling,numsite,realdata,nbias=2){
		expe=list()
		biases=seq(0,0.9,length.out=nbias)
		names(biases)=paste0(biases,"bias")
		expe[["data"]]=read.csv(file.path(fold,"agents.csv"),sep=";")
		expe[["div"]]=t(agentWith(expe[["data"]],breaks=nrow(realdata),type="div"))
		expe[["count"]]=t(agentWith(expe[["data"]],breaks=nrow(realdata),type="count"))
		expe[["simpscore"]]=simpscore(expe[["div"]],realdata)
		expe[["scores"]]= sapply(biases,function(i){print(i);return(computeScoresDat(expe[["data"]],sample=repeatsampling,numsite=numsite,bias=i,realdata=realdata))})
		return(expe)
}


plotSimpsonVsZscores <- function()plot(sapply(allbias,function(i)(i[["simpscore"]])),sapply(allbias,function(i)mean(i[["scores"]][["zscores",10]])))

plotZscoreAndSimp <- function(dat,ranked=T,...){
	plot(c(1,ncol(dat)),range(sapply(dat["zscores",],range)),type="n",ylab="zscores",xlab="rank (given by mean zscore)",...)
	sapply(1:ncol(dat),function(i)try(vioplot(dat[["zscores",i]],col=NA,at=i,add=T)))
}

getZscores <- function(allb,fun=mean){
	allSd=sapply(allbias,function(u)sapply(u[["scores"]]["zscores",],fun))
	allclean= t(simplify2array(allSd[sapply(allSd,function(i)length(i)>0)]))  
	return(allclean)
}

plotScoreWrtBias <- function(allb,scores="zscores",fun=mean){
	tableres=getZscores(allb,fun)
	plot(1:10,1:10,ylim=range(tableres),xlim=c(0,ncol(tableres)),type="n",ylab=deparse(substitute(fun)),xlab="% of non random cities")
	apply(tableres,1,lines)
}

#return the score in an experiment folder
getscoreFold <- function(fold)read.csv(paste0(fold,"/score.txt"),header=F)  
getAllScoreFolds <- function(listfold)sapply(listfold,function(u)(tryCatch(getscoreFold(u)$V1,error=function(err){NA})))

#given the folder of an ABC experiment it a list with the sore for each experiment
getListScoreFold <- function(fold){
	rawScores = getAllScoreFolds(list.dirs(fold))
       	return(simplify2array(rawScores[sapply(rawScores,function(u)!is.na(u))]))
}


getMin <- function(){
row=names(which.min(apply( multiexp$count,1,min,na.rm=T)) )
col=names(which.min(apply( multiexp$count,2,min,na.rm=T)) )

row=names(which.max(apply( multiexp$div,1,max,na.rm=T)) )
col=names(which.max(apply( multiexp$div,2,max,na.rm=T)) )

wors=cbind(multiexp$div[row,col],absdif$div[row,col])
multiexp$count=score$zscore$nonprop$dis
multiexp$div=score$zscore$nonprop$div
absdif$count=score$absdiff$nonprop$dis
absdif$div=score$absdiff$nonprop$div
absdifPROP$div=score$absdiff$prop$div
absdifPROP$count=score$absdiff$prop$dis
zscorePROP$count=score$zscore$prop$dis
zscorePROP$div=score$zscore$prop$div
pdf("~/presModel/zscorePATTERNtest.pdf")
plot(multiexp$count ~ multiexp$div,col=colyear[rownames(multiexp$count)],pch=20)
dev.off()
pdf("~/presModel/zscorePROPtest.pdf")
plot(multiexp$count ~ zscorePROP$count,col=colyear[rownames(zscorePROP$count)],pch=20)
dev.off()
pdf("~/presModel/absPATTERNtest.pdf")
plot(absdif$count ~ absdif$div,col=colyear[rownames(absdifPROP$count)],pch=20)
dev.off()
pdf("~/presModel/2scoretestCOUNT.pdf")
plot(multiexp$count ~ absdif$count,col=colyear[rownames(multiexp$count)],pch=20,ylab="zscore",xlab="absolute diff",main="#good/site")
dev.off()
pdf("~/presModel/2scoretestDIV.pdf")
plot(multiexp$div ~ absdif$div,col=colyear[rownames(multiexp$count)],pch=20,ylab="zscore",xlab="absolute diff",main="#site/good")
dev.off()
points(best,col="red",cex=2)

cold=names(which.min(apply( multiexp$div,2,min,na.rm=T)) )
rowd=names(which.max(apply( multiexp$div,1,max,na.rm=T)) )
bestd=cbind(multiexp$div[rowd,cold],multiexp$count[rowd,cold])
pdf("~/presModel/spotSimu.pdf")
plot(multiexp$count ~ multiexp$div,col=colyear[rownames(multiexp$count)],pch=20)
points(worst,col="red",cex=2)
dev.off()

pdf("~/presModel/spotSimuD.pdf")
plot(multiexp$count ~ multiexp$div,col=colyear[rownames(multiexp$count)],pch=20)
points(bestd,col="red",cex=2)
dev.off()
pdf("~/presModel/spotted1Dist.pdf")
plotFromSel(list(year=row,fold=col),pattern="dis")
dev.off()
pdf("~/presModel/spotted1Div.pdf")
plotFromSel(list(year=row,fold=col),pattern="div")
dev.off()

pdf("~/presModel/spotted2Dist.pdf")
plotFromSel(list(year=rowd,fold=cold),pattern="dis")
dev.off()
pdf("~/presModel/spotted2Div.pdf")
plotFromSel(list(year=rowd,fold=cold),pattern="div")
dev.off()


	
bdzs=getFolderMaxFromList(multiexp[["di"]])
gdzs=getFolderMinFromList(multiexp$count)
bczs=getFolderMaxFromList(multiexp$count)
gczs=getFolderMinFromList(multiexp$count)
par(mfrow=c(2,4))
plotMinMaxFromList(absdifPROP,type="div",side="good")
plotMinMaxFromList(absdifPROP,type="div",side="good")
plotMinMaxFromList(zscorePROP,type="count",side="good")
plotMinMaxFromList(absdif,type="count",side="good")
plotMinMaxFromList(multiexp,type="div",side="bad")
plotMinMaxFromList(zscorePROP,type="count",side="good")
plotMinMaxFromList(multiexp,type="count",side="bad")
plotMinMaxFromList(multiexp,type="count",side="good")

plotSiteWithGood(realdatadiversities[[gdzs$year]])
plotSiteWithGood(getFoldExpInfos(bczs$fold,nbias=1,realdata=realdatadistributions[[bczs$year]],numsite=40,repeatsampling=1)$counted)
}

##given a list of folder with associated scores plot the best or the worst simulation given a score `type`
plotMinMaxFromList <- function(inlist,numsite=200,type="div",side="bad"){
	select=NULL
	if(side=="bad")
		select = getFolderMaxFromList(inlist[[type]])
	if(side=="good")
		select = getFolderMinFromList(inlist[[type]])
	print(select)
	dataworst=getDatas(select$fold)[[1]][[1]]
	getcount=t(agentWith(dataworst,breaks=as.numeric(select$year),numsite=numsite,type=type,bias=1))
	plotSiteWithGood(getcount) 
} 

plotFromSel <- function(select,numsite=40,pattern="div"){
	print(select)
	print(as.numeric(select$year))
	getcount=agentWith(read.csv(paste0(select$fold,"/agents.csv"),sep=";"),numperiods=as.numeric(select$year),numsite=numsite,bias=1,proportion=F,pattern=pattern)
	print(agentWith)
	print(getcount)
	plotSiteWithGood(getcount) 
} 

printGraph <- function(){
getFolderMinFromList <- function(inlist)list(fold=names(which.min(apply(inlist,2,min,na.rm=T)) ),year=names(which.min(apply(inlist,1,min,na.rm=T)) ))
getFolderMaxFromList <- function(inlist)list(fold=names(which.max(apply(inlist,2,max,na.rm=T)) ),year=names(which.max(apply(inlist,1,max,na.rm=T)) ))
dis
pdf("~/presModel/zscorePSZIE.pdf")
plot(1,1,ylim=range(enscore$absdiff$prop$dis,na.rm=T),xlim=c(.5,length(rownames(enscore$absdiff$prop$dis))+.5),axes=F,ylab=" score" ,col=alpha("black",.5),xlab="size of P (y)" )
axis(2)
axis(1,1:length(rownames(multiexp$div)),label=rownames(multiexp$div))
apply(enscore$absdiff$prop$dis,2,lines,col=alpha("black",.5),lwd=.1)
dev.off()

pdf("~/presModel/zscorePSZIEcount.pdf")
plot(1,1,ylim=range(multiexp$count,na.rm=T),xlim=c(.5,length(rownames(multiexp$count))+.5),axes=F,ylab=" score" ,col=alpha("black",.5),xlab="size of P (y)" )
axis(2)
axis(1,1:length(rownames(multiexp$count)),label=rownames(multiexp$count))
apply(multiexp$count,2,lines,col=alpha("black",.5),lwd=.5)
dev.off()

pdf("~/presModel/absdifPSZIE.pdf")
plot(1,1,ylim=range(absdif$div,na.rm=T),xlim=c(.5,length(rownames(absdif$div))+.5),axes=F,ylab=" score" ,col=alpha("black",.5),xlab="size of P (y)" )
axis(2)
axis(1,1:length(rownames(absdif$div)),label=rownames(absdif$div))
apply(absdif$div,2,lines,col=alpha("black",.5),lwd=.5)
dev.off()

pdf("~/presModel/absdifPSZIEcount.pdf")
plot(1,1,ylim=range(absdif$count,na.rm=T),xlim=c(.5,length(rownames(absdif$count))+.5),axes=F,ylab=" score" ,col=alpha("black",.1),xlab="size of P (y)" )
axis(2)
axis(1,1:length(rownames(absdif$count)),label=rownames(absdif$count))
apply(absdif$count,2,lines,col=alpha("black",.5),lwd=.5)
dev.off()

pdf("~/presModel/absdifPROP_propPSZIE.pdf")
plot(1,1,ylim=range(absdifPROP$div,na.rm=T),xlim=c(.5,length(rownames(absdifPROP$div))+.5),axes=F,ylab=" score" ,col=alpha("black",.1),xlab="size of P (y)" )
axis(2)
axis(1,1:length(rownames(absdifPROP$div)),label=rownames(absdifPROP$div))
apply(absdifPROP$div,2,lines,col=alpha("black",.1),lwd=.05)
dev.off()

dev.new()

plot(1,ylim=c(0,1))
size=rev(1:length(years))
names(size)=years
sapply(years,function(u){par(new=T);plotSiteWithGood(getRealDataCount(numperiods=u,pattern="dis",proportion=T),ylim=c(0,1),axes=F,legend=c(),alph=1/(size[as.character(u)]),lwd=size[as.character(u)])})  
sapply(years,function(u){par(new=T);plotSiteWithGood(getRealDataCount(numperiods=u,pattern="div",proportion=T),ylim=c(0,1),axes=F,legend=c(),alph=1,lwd=.5)})  

cole=names(which.min(apply( enscore$absdiff$nonprop$dis,2,min,na.rm=T)) )
rowe=names(which.min(apply( enscore$absdiff$nonprop$dis,1,min,na.rm=T)) )
beste=cbind(enscore$absdiff$nonprop$dis[rowe,cole],$count[rowe,cole])

plot(enscore$absdif$prop$dis[,11],enscore$absdif$prop$div[,11],col=colyear[rownames(enscore$absdif$prop$div)],pch=20)
plot(enscore$absdif$prop$dis[,11],enscore$absdif$prop$div[,11])
}

getUnderValue <- function(inlist,val){
	years=dimnames(inlist)[[1]]
	folds=dimnames(inlist)[[2]]
	rscores=c()
	ryears=c()
	rfolds=c()


	for(year in years){
		for(fold in folds){
			score=inlist[year,fold]
			if(!is.na(score)){
			if(score < val){
				rscores=c(rscores,score)
				ryears=c(ryears,year)
				rfolds=c(rfolds,fold)
			}
			}
		}
	}
	return(cbind.data.frame(year=ryears,fold=rfolds,score=rscores))



}
