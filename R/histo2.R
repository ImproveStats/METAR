histo2 <-
function(x,main)
{
	to.plot2 <- split(x,as.character(x$Rep))
	nRep <- length(to.plot2)
	Freq <- c()
	for(k2 in 1:nRep)	Freq <- c(Freq,hist(to.plot2[[k2]]$y,freq=F,plot=FALSE)$density)

	par(mfrow = c(ifelse(nRep<=2,1,2),2), mar = c(2.2,1,1,0), oma = c(2,3.3,4.3,2))
	for(k2 in 1:nRep){
		if(k2<=4){
			a <- ifelse(nRep<=2,1.2,1.25)
			hist(to.plot2[[k2]]$y,freq=FALSE,ylim=c(0,a*max(Freq)),
				main="",xlab="",ylab="",yaxt="n",col="orange")
			if(k2%%2==1) axis(2,round(seq(0,round(max(Freq),1),length=5),1))
			abline(h=1.18*max(Freq))
			mtext(names(to.plot2)[k2],line=-1.1,outer=F)
			box()
		}
	}
	mtext(main, line = 0.5,outer = TRUE, cex = 1.3,font=2)
	mtext("Percentage",line = 1.8,outer = TRUE, cex = 1,side=2)
}
