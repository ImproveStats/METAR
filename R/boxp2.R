boxp2 <-
function(to.plot,dec=dec,main=main,ylab,cex0)
{
	maxLength <- ifelse(max(nchar(names(to.plot)))==0,1,max(nchar(names(to.plot))))
	L0 <- length(to.plot)
	medias <- round(unlist(lapply(to.plot,mean,na.rm=TRUE)),dec)
	sdesv <- round(unlist(lapply(to.plot,sd,na.rm=TRUE)),dec)
	mins <- suppressWarnings(round(unlist(lapply(to.plot,min,na.rm=TRUE)),dec))
	maxs <- suppressWarnings(round(unlist(lapply(to.plot,max,na.rm=TRUE)),dec))
	medians <- round(unlist(lapply(to.plot,median,na.rm=TRUE)),dec)
	maxs[is.infinite(maxs)] <- NA
	mins[is.infinite(mins)] <- NA
	maximo <- suppressWarnings(max(maxs,na.rm=TRUE))
	minimo <- suppressWarnings(min(mins,na.rm=TRUE))
	ancho <- (maximo-minimo)/5
	mins[is.na(mins)] <- "-"
	maxs[is.na(maxs)] <- "-"
	medias[is.na(medias)] <- "-"
	medians[is.na(medians)] <- "-"
	sdesv[is.na(sdesv)] <- "-"

	a <- ifelse(maxLength>12,7.2,4+log(maxLength))
	par(mar=c(a, 4.1, 4.1,2.1), xpd=FALSE)
	boxplot(to.plot,main=main,xlab="",ylab=ylab,col="orange",
	ylim=c(minimo,maximo+1.30*ancho),yaxt="n",xaxt="n",xlim=c(0,L0+0.35))
	abline(h=maximo+0.3*ancho)
	ajuste <- 0.5
	index <- nchar(names(to.plot))>12
      if(sum(index)>0) names(to.plot)[index] <- paste(substr(names(to.plot)[index],1,12),"...",sep="") 
	axis(1,1:L0,labels=names(to.plot),las=2,cex.axis=0.8)
	axis(2,round(seq(minimo,maximo,length=6),1))
	text(c(0.1,1:L0),maximo+0.5*ancho,c("Min",mins),cex=cex0,adj=c(ajuste,NA))
	text(c(0.1,1:L0),maximo+0.73*ancho,c("Mean",medias),cex=cex0,adj=c(ajuste,NA))
	text(c(0.1,1:L0),maximo+0.96*ancho,c("Median",medians),cex=cex0,adj=c(ajuste,NA))
	text(c(0.1,1:L0),maximo+1.19*ancho,c("Max",maxs),cex=cex0,adj=c(ajuste,NA))
	text(c(0.1,1:L0),maximo+1.42*ancho,c("StdDev",sdesv),cex=cex0,adj=c(ajuste,NA))
	for(k in 1:L0)	rect(k-0.5,maximo+0.3*ancho,k-0.5,maximo+1.55*ancho)
}
