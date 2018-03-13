 library(parallel)
 source("function.R") 

load("datalist")


	years=seq(10,100,20)
	names(years)=years
    
  score=lapply(list(absdiff=absdiff,zscore=zscore),function(diff)
	       lapply(list(nonprop=F,prop=T),function(p)
		      lapply(list(dis="dis",div="div"),function(rdt){
                     print(paste(deparse(substitute(diff)),p,rdt))
                     getAllScores(datalist[1:95],allperiods=years,diff=diff,pattern=rdt,proportion=p)
                    }
			     )
		      )
	       )
    save(score,file="scores.Rout")
