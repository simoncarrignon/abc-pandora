#file copied from previous script to draw the results of the ABC runs

legend  <- function (x, y = NULL, legend, fill = NULL, col = par("col"), 
    border = "black", lty, lwd, pch, angle = 45, density = NULL, 
    bty = "o", bg = par("bg"), box.lwd = par("lwd"), box.lty = par("lty"), 
    box.col = par("fg"), pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd, 
    xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0, 
        0.5), text.width = NULL, text.col = par("col"), text.font = NULL, 
    merge = do.lines && has.pch, trace = FALSE, plot = TRUE, 
    ncol = 1, horiz = FALSE, title = NULL, inset = 0, xpd, title.cex = cex,title.col = text.col, 
    title.adj = 0.5, seg.len = 2) 
{
    if (missing(legend) && !missing(y) && (is.character(y) || 
        is.expression(y))) {
        legend <- y
        y <- NULL
    }
    mfill <- !missing(fill) || !missing(density)
    if (!missing(xpd)) {
        op <- par("xpd")
        on.exit(par(xpd = op))
        par(xpd = xpd)
    }
    title <- as.graphicsAnnot(title)
    if (length(title) > 1)
        stop("invalid 'title'")
    legend <- as.graphicsAnnot(legend)
    n.leg <- if (is.call(legend))
        1
    else length(legend)
    if (n.leg == 0)
        stop("'legend' is of length 0")
    auto <- if (is.character(x))
        match.arg(x, c("bottomright", "bottom", "bottomleft",
            "left", "topleft", "top", "topright", "right", "center"))
    else NA
    if (is.na(auto)) {
        xy <- xy.coords(x, y)
        x <- xy$x
        y <- xy$y
        nx <- length(x)
        if (nx < 1 || nx > 2) 
            stop("invalid coordinate lengths")
    }
    else nx <- 0
    xlog <- par("xlog")
    ylog <- par("ylog")
    rect2 <- function(left, top, dx, dy, density = NULL, angle, 
        ...) {
        r <- left + dx
        if (xlog) {
            left <- 10^left
            r <- 10^r
        }
        b <- top - dy
        if (ylog) {
            top <- 10^top
            b <- 10^b
        }
        rect(left, top, r, b, angle = angle, density = density, 
            ...)
    }
    segments2 <- function(x1, y1, dx, dy, ...) {
        x2 <- x1 + dx
        if (xlog) {
            x1 <- 10^x1
            x2 <- 10^x2
        }
        y2 <- y1 + dy
        if (ylog) {
            y1 <- 10^y1
            y2 <- 10^y2
        }
        segments(x1, y1, x2, y2, ...)
    }
    points2 <- function(x, y, ...) {
        if (xlog) 
            x <- 10^x
        if (ylog) 
            y <- 10^y
        points(x, y, ...)
    }
    text2 <- function(x, y, ...) {
        if (xlog) 
            x <- 10^x
        if (ylog) 
            y <- 10^y
        text(x, y, ...)
    }
    if (trace) 
        catn <- function(...) do.call("cat", c(lapply(list(...), 
            formatC), list("\n")))
    cin <- par("cin")
    Cex <- cex * par("cex")
    if (is.null(text.width)) 
        text.width <- max(abs(strwidth(legend, units = "user", 
            cex = cex, font = text.font)))
    else if (!is.numeric(text.width) || text.width < 0) 
        stop("'text.width' must be numeric, >= 0")
    xc <- Cex * xinch(cin[1L], warn.log = FALSE)
    yc <- Cex * yinch(cin[2L], warn.log = FALSE)
    if (xc < 0) 
        text.width <- -text.width
    xchar <- xc
    xextra <- 0
    yextra <- yc * (y.intersp - 1)
    ymax <- yc * max(1, strheight(legend, units = "user", cex = cex)/yc)
    ychar <- yextra + ymax
    if (trace) 
        catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra, 
            ychar))
    if (mfill) {
        xbox <- xc * 0.8
        ybox <- yc * 0.5
        dx.fill <- xbox
    }
    do.lines <- (!missing(lty) && (is.character(lty) || any(lty > 
        0))) || !missing(lwd)
    n.legpercol <- if (horiz) {
        if (ncol != 1) 
            warning(gettextf("horizontal specification overrides: Number of columns := %d", 
                n.leg), domain = NA)
        ncol <- n.leg
        1
    }
    else ceiling(n.leg/ncol)
    has.pch <- !missing(pch) && length(pch) > 0
    if (do.lines) {
        x.off <- if (merge) 
            -0.7
        else 0
    }
    else if (merge) 
        warning("'merge = TRUE' has no effect when no line segments are drawn")
    if (has.pch) {
        if (is.character(pch) && !is.na(pch[1L]) && nchar(pch[1L], 
            type = "c") > 1) {
            if (length(pch) > 1) 
                warning("not using pch[2..] since pch[1L] has multiple chars")
            np <- nchar(pch[1L], type = "c")
            pch <- substr(rep.int(pch[1L], np), 1L:np, 1L:np)
        }
        if (!is.character(pch)) 
            pch <- as.integer(pch)
    }
    if (is.na(auto)) {
        if (xlog) 
            x <- log10(x)
        if (ylog) 
            y <- log10(y)
    }
    if (nx == 2) {
        x <- sort(x)
        y <- sort(y)
        left <- x[1L]
        top <- y[2L]
        w <- diff(x)
        h <- diff(y)
        w0 <- w/ncol
        x <- mean(x)
        y <- mean(y)
        if (missing(xjust)) 
            xjust <- 0.5
        if (missing(yjust)) 
            yjust <- 0.5
    }
    else {
        h <- (n.legpercol + (!is.null(title))) * ychar + yc
        w0 <- text.width + (x.intersp + 1) * xchar
        if (mfill) 
            w0 <- w0 + dx.fill
        if (do.lines) 
            w0 <- w0 + (seg.len + x.off) * xchar
        w <- ncol * w0 + 0.5 * xchar
        if (!is.null(title) && (abs(tw <- strwidth(title, units = "user", 
            cex = cex) + 0.5 * xchar)) > abs(w)) {
            xextra <- (tw - w)/2
            w <- tw
        }
        if (is.na(auto)) {
            left <- x - xjust * w
            top <- y + (1 - yjust) * h
        }
        else {
            usr <- par("usr")
            inset <- rep_len(inset, 2)
            insetx <- inset[1L] * (usr[2L] - usr[1L])
            left <- switch(auto, bottomright = , topright = , 
                right = usr[2L] - w - insetx, bottomleft = , 
                left = , topleft = usr[1L] + insetx, bottom = , 
                top = , center = (usr[1L] + usr[2L] - w)/2)
            insety <- inset[2L] * (usr[4L] - usr[3L])
            top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
                h + insety, topleft = , top = , topright = usr[4L] - 
                insety, left = , right = , center = (usr[3L] + 
                usr[4L] + h)/2)
        }
    }
    if (plot && bty != "n") {
        if (trace) 
            catn("  rect2(", left, ",", top, ", w=", w, ", h=", 
                h, ", ...)", sep = "")
        rect2(left, top, dx = w, dy = h, col = bg, density = NULL, 
            lwd = box.lwd, lty = box.lty, border = box.col)
    }
    xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1), 
        rep.int(n.legpercol, ncol)))[1L:n.leg]
    yt <- top - 0.5 * yextra - ymax - (rep.int(1L:n.legpercol, 
        ncol)[1L:n.leg] - 1 + (!is.null(title))) * ychar
    if (mfill) {
        if (plot) {
            if (!is.null(fill)) 
                fill <- rep_len(fill, n.leg)
            rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox, 
                col = fill, density = density, angle = angle, 
                border = border)
        }
        xt <- xt + dx.fill
    }
    if (plot && (has.pch || do.lines)) 
        col <- rep_len(col, n.leg)
    if (missing(lwd) || is.null(lwd)) 
        lwd <- par("lwd")
    if (do.lines) {
        if (missing(lty) || is.null(lty)) 
            lty <- 1
        lty <- rep_len(lty, n.leg)
        lwd <- rep_len(lwd, n.leg)
        ok.l <- !is.na(lty) & (is.character(lty) | lty > 0) & 
            !is.na(lwd)
        if (trace) 
            catn("  segments2(", xt[ok.l] + x.off * xchar, ",", 
                yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
        if (plot) 
            segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len * 
                xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l], 
                col = col[ok.l])
        xt <- xt + (seg.len + x.off) * xchar
    }
    if (has.pch) {
        pch <- rep_len(pch, n.leg)
        pt.bg <- rep_len(pt.bg, n.leg)
        pt.cex <- rep_len(pt.cex, n.leg)
        pt.lwd <- rep_len(pt.lwd, n.leg)
        ok <- !is.na(pch)
        if (!is.character(pch)) {
            ok <- ok & (pch >= 0 | pch <= -32)
        }
        else {
            ok <- ok & nzchar(pch)
        }
        x1 <- (if (merge && do.lines) 
            xt - (seg.len/2) * xchar
        else xt)[ok]
        y1 <- yt[ok]
        if (trace) 
            catn("  points2(", x1, ",", y1, ", pch=", pch[ok], 
                ", ...)")
        if (plot) 
            points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok], 
                bg = pt.bg[ok], lwd = pt.lwd[ok])
    }
    xt <- xt + x.intersp * xchar
    if (plot) {
        if (!is.null(title)) 
            text2(left + w * title.adj, top - ymax, labels = title, 
                adj = c(title.adj, 0), cex = title.cex, col = title.col)
        text2(xt, yt, labels = legend, adj = adj, cex = cex, 
            col = text.col, font = text.font)
    }
    invisible(list(rect = list(w = w, h = h, left = left, top = top), 
        text = list(x = xt, y = yt)))
}



##from a path with epsilon_*.csv results plot the epsilon for each  \theta
allFromPath <- function(pathtoepsilon){
	ll=getlistParticlesFromPath(pathtoepsilon,epsilon)
	plotAllDensities(ll)
}


#plot \theta_a wrt to \theta_2 for all epsilon of a particle list
plotcov <- function(listofpart,a,b){
	par(mfrow=c(1,length(listofpart)))
	lapply(listofpart,function(u)plot(u[,a] ~ u[,b],ylab=a,xlab=b))
	par(mfrow=c(1,1))
}

#plot thes densites of all epsilon for all thethas of a list of particles
plotAllDensities <- function(table,...){
	var=colnames(table[[1]])[!colnames(table[[1]])%in% c("epsilon","score")]
	par(mfrow=c(2,length(var)-3))

	for(v in var){
		plotDensities(table,v,names(table))
	}
	par(mfrow=c(1,1))

}

#plot densities of all thetas for the epsilons in the vector `epsilon`
plotDensities <- function(table,param,epsilon,...){
	#we assume table1 is a "priorlike" function
	epsilon=epsilon[2:length(epsilon)]
	print(epsilon)
	print(param)
	prior=table[[1]][,param]
	rng=range(prior)
	print(rng)
	from=rng[1]
	to=rng[2]
	htcol=topo.colors(length(epsilon),alpha=1) 
	names(htcol)=epsilon
	htcolF=topo.colors(length(epsilon),alpha=.5)
	#htcolF=c(rgb(34, 139, 34,127.5,maxColorValue=255),rgb(178, 255, 255,127.5,maxColorValue=255))
	names(htcolF)=epsilon
	htcolF=c(htcolF,"red")
	names(htcolF)[length(htcolF)]="prior"
	listParticles=table[2:length(table)]
	names(listParticles)=epsilon
	densities=lapply(listParticles,function(i){density(i[,param],from=0)})#,from=from,to=to)})
	densitiesPrio=density(prior,from=from,to=to)
	names(densities)=epsilon
	rangex=range(lapply(densities,function(i)range(i$x)),densitiesPrio$x)
	rangey=range(lapply(densities,function(i)range(i$y)),densitiesPrio$y*1.1)
	par(mar=c(5,5,1,1))
	plot(density(listParticles[[1]][,param]),ylim=rangey,xlim=rangex,type="n",main="", xlab=substitute(p,list(p=param)),...)
	polygon(c(from,densitiesPrio$x,to),c(0,densitiesPrio$y,0),col=htcolF[length(htcolF)],lwd=2)
	lapply(seq_along(densities),function(i){
	       polygon(c(rangex[1],densities[[i]]$x,0),c(0,densities[[i]]$y,0),col=htcolF[names(densities)[i]],lwd=2)#,density=20,angle=45*i,border=htcol[names(densities)[i]])
	       #	   abline(v=mean(densities[[i]]$x),col=htcol[names(densities)[i]])
	       #	   text(mean(densities[[i]]$x),0,names(densities)[i],col=htcol[names(densities)[i]])
})
	text(from,max(densitiesPrio$y)+.05*max(densitiesPrio$y),substitute(prior:param %~% italic(u)(from,to),list(param=param,from=round(from),to=round(to))),cex=.8)
	legend("topright",legend=names(htcolF),fill=htcolF,title=expression(epsilon),inset=.05,title.cex=1.8)
}

plotDensitiesFrompath <- function(path,param,epsilon,from,to,...){

    htcol=topo.colors(length(epsilon),alpha=1) 
    names(htcol)=epsilon
    htcolF=topo.colors(length(epsilon),alpha=.5)
    #htcolF=c(rgb(34, 139, 34,127.5,maxColorValue=255),rgb(178, 255, 255,127.5,maxColorValue=255))
    names(htcolF)=epsilon
    htcolF=c(htcolF,"red")
    names(htcolF)[length(htcolF)]="prior"
    listParticles=lapply(epsilon,function(eps){print(eps);cbind(read.csv(paste(path,"result_",eps,".csv",sep="") ),epsilon=eps)})
    names(listParticles)=epsilon
    print(listParticles)
    densities=lapply(listParticles,function(i){density(i[,param],from=0)})#,from=from,to=to)})
    rdnm=runif(5000,from,to)
    densitiesPrio=density(rdnm,from=from,to=to)
    names(densities)=epsilon
    rangex=range(lapply(densities,function(i)range(i$x)),densitiesPrio$x)
    rangey=range(lapply(densities,function(i)range(i$y)),densitiesPrio$y)
    par(mar=c(5,5,1,1))
    plot(density(listParticles[[1]][,param]),ylim=rangey,xlim=rangex,type="n",main="", xlab=substitute(p,list(p=param)),...)
    polygon(c(from,densitiesPrio$x,to),c(0,densitiesPrio$y,0),col=htcolF[length(htcolF)],lwd=2)
    lapply(seq_along(densities),function(i){
	   polygon(c(from,densities[[i]]$x,to),c(0,densities[[i]]$y,0),col=htcolF[names(densities)[i]],lwd=2)#,density=20,angle=45*i,border=htcol[names(densities)[i]])
#	   abline(v=mean(densities[[i]]$x),col=htcol[names(densities)[i]])
#	   text(mean(densities[[i]]$x),0,names(densities)[i],col=htcol[names(densities)[i]])
	})
    text(from,max(densitiesPrio$y)+.05*max(densitiesPrio$y),substitute(prior:param %~% italic(u)(from,to),list(param=param,from=from,to=to)))
    legend("topright",legend=names(htcolF),fill=htcolF)
    return(listParticles)
}

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
#	save(datalist,file="datalist")
#	load("realdatadistributions")
#	load("realdatadiversities")
#	load("listoffoldsccore")
#	load("datalist")

	absdif=list()
	multiexp=list()

	years=seq(10,100,5)
	names(years)=seq(10,100,5)

	datalist=getDatas(names(listoffoldsccore[sample.int(length(listoffoldsccore),1500)]))

	print("done datalist===")

	multiexp[["count"]]=sapply(datalist,function(eij){print(".");sapply(years,function(y){tryCatch(zscore(t(agentWith(eij$data,breaks=y,numsite=40,type="count")),realdatadistributions[[as.character(y)]]),error=function(err){NA})})})
	print("done count ===")
	multiexp[["div"]]=	sapply(datalist,function(eij){print(".");sapply(years,function(y){tryCatch(zscore(t(agentWith(eij$data,breaks=y,numsite=40,type="div")),realdatadiversities[[as.character(y)]]),error=function(err){NA})})})
	print("done div ===")


	absdif[["count"]]=sapply(datalist,function(eij){print(".");sapply(years,function(y){tryCatch(absdiff(t(agentWith(eij$data,breaks=y,numsite=40,type="count")),realdatadistributions[[as.character(y)]]),error=function(err){NA})})})
	print("===")
	absdif[["div"]]=sapply(datalist,function(eij){print(".");sapply(years,function(y){tryCatch(absdiff(t(agentWith(eij$data,breaks=y,numsite=40,type="div")),realdatadiversities[[as.character(y)]]),error=function(err){NA})})})


	colyear=topo.colors(length(years))
	names(colyear)=years
	plot(multiexp$count ~ multiexp$div,col=colyear[rownames(multiexp$count)],pch=20)
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

	plot(1,1,ylim=range(multiexp$div,na.rm=T),xlim=c(0,12))  
	apply(multiexp$div,2,lines) 
	plot(1,1,ylim=range(multiexp$count,na.rm=T),xlim=c(0,12))  
	apply(multiexp$count,2,lines) 

	plot(1,1,ylim=range(absdif$div,na.rm=T),xlim=c(0,12))  
	apply(absdif$div,2,lines) 
	plot(1,1,ylim=range(absdif$count,na.rm=T),xlim=c(0,12))  
	apply(absdif$count,2,lines) 
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

generateRealCount <- function(granularity,type="count"){
	
	data=read.csv("~/data_per_year.csv")
	data$goods=data$Fabric
	data$date=cut(data$date,breaks=granularity) 
	if(type=="count")
		return(sapply( levels(data$goods) , function(g)sapply(sort(unique(data$date)),function(ts){length(unique(data$Location_ascii[data$date == ts & data$goods == g]))})) )
	if(type=="div")
		return(t(sapply( levels(data$date) , function(ts) table(factor(sapply(unique(data$Location_ascii),function(ag)length(unique((data$goods[data$Location_ascii==ag & data$date == ts])))) ,levels=0:length(unique(data$goods)))))))

}

getMin <- function(){
row=names(which.min(apply( multiexp$count,1,min,na.rm=T)) )
best=cbind(multiexp$div[row,col],multiexp$count[row,col])
plot(multiexp$count ~ multiexp$div,col=colyear[rownames(multiexp$count)],pch=20)
plot(multiexp$count ~ absdif$count,col=colyear[rownames(multiexp$count)],pch=20)
plot(multiexp$div ~ absdif$div,col=colyear[rownames(multiexp$count)],pch=20)
points(best,col="red",cex=2)

col=names(which.max(apply( multiexp$count,1,max,na.rm=T)) )
row=names(which.max(apply( multiexp$count,1,max,na.rm=T)) )
worst=cbind(multiexp$div[row,col],multiexp$count[row,col])
points(worst,col="red",cex=2)

	
bdzs=getFolderMaxFromList(multiexp[["di"]])
gdzs=getFolderMinFromList(multiexp$count)
bczs=getFolderMaxFromList(multiexp$count)
gczs=getFolderMinFromList(multiexp$count)
par(mfrow=c(2,4))
plotMinMaxFromList(absdif,type="div",side="bad")
plotMinMaxFromList(absdif,type="div",side="good")
plotMinMaxFromList(absdif,type="count",side="bad")
plotMinMaxFromList(absdif,type="count",side="good")
plotMinMaxFromList(multiexp,type="div",side="bad")
plotMinMaxFromList(multiexp,type="div",side="good")
plotMinMaxFromList(multiexp,type="count",side="bad")
plotMinMaxFromList(multiexp,type="count",side="good")

plotSiteWithGood(realdatadiversities[[gdzs$year]])
plotSiteWithGood(getFoldExpInfos(bczs$fold,nbias=1,realdata=realdatadistributions[[bczs$year]],numsite=40,repeatsampling=1)$counted)
}

plotMinMaxFromList <- function(inlist,numsite=40,type="div",side="bad"){
	select=NULL
	if(side=="bad")
		select = getFolderMaxFromList(inlist[[type]])
	if(side=="good")
		select = getFolderMinFromList(inlist[[type]])
	print(select)
	dataworst=getDatas(select$fold)[[1]][[1]]
	getcount=t(agentWith(dataworst,breaks=as.numeric(worst$year),numsite=numsite,type=type,bias=1))
	plotSiteWithGood(getcount) 
} 

getFolderMinFromList <- function(inlist)list(fold=names(which.min(apply(inlist,2,min,na.rm=T)) ),year=names(which.min(apply(inlist,1,min,na.rm=T)) ))
getFolderMaxFromList <- function(inlist)list(fold=names(which.max(apply(inlist,2,max,na.rm=T)) ),year=names(which.max(apply(inlist,1,max,na.rm=T)) ))

