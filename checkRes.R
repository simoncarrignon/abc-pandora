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

plotAllDensities <- function(table,...){
	var=colnames(table[[1]])[!colnames(table[[1]])%in% c("epsilon","score")]
	par(mfrow=c(2,length(var)-3))

	for(v in var){
		plotDensities(table,v,names(table))
	}
	par(mfrow=c(1,1))

}

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

path="./"
epsilon=c("1000.0","0.25","0.2036","0.1658")
ll=plotDensitiesFrompath(path,"nstep",epsilon,150,360)
llbis=lapply(ll,function(u) {u$ratio = (u$cstep)/u$nstep; return(u)})

plotAllDensities(llbis)



