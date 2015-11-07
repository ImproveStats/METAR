biplot2 <-
function(PC,main="Hola",cex.axis=0.75,cex=0.8)
{	
  a1 <- max(abs(as.vector(PC$loadings[,1])))
  a2 <- max(abs(as.vector(PC$loadings[,2])))
	vari <- PC$sdev^2
	varexpl <- round(100*vari/sum(vari),2)
	xlab <- paste("PC 1 (",varexpl[1],"%)",sep="")
	ylab <- paste("PC 2 (",varexpl[2],"%)",sep="")
	biplot(PC,xlim=c(-a1,a1),cex.axis=cex.axis,cex=cex,xlab=xlab,ylab=ylab,ylim=c(-a2,a2),main=main)# ,xaxt='n',yaxt='n')
	abline(v=0,lty=2)
	abline(h=0,lty=2)	
}
