 library(Rmpi)
 source("function.R") 



	years=seq(10,100,5)
	names(years)=years
    load("fname")
    
    print("YOLOLO")

    mpi.spawn.Rslaves(nslaves=479)

    mpi.bcast.cmd( id <- mpi.comm.rank() )
    mpi.bcast.cmd( ns <- mpi.comm.size() )
    mpi.bcast.cmd( host <- mpi.get.processor.name() )
    mpi.bcast.cmd(source("function.R"))

     
    # Test computations
     

  score=lapply(list(absdiff=absdiff,zscore=zscore),function(diff)
           lapply(list(nonprop=F,prop=T),function(p)
    	      lapply(list(dis="dis",div="div"),function(rdt){
                     getAllScores(fname,allperiods=years,diff=diff,pattern=rdt,proportion=p,par="mpi")
                    }
    		     )
    	      )
           )
    save(score,file="scores.Rout")

    # Tell all slaves to close down, and exit the program
    mpi.close.Rslaves(dellog = FALSE)
    mpi.quit()
