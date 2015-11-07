biplot3 <-
function(PC,main="Biplot")
{
	#X <- PC$loadings[,1:2]      ###
	X <- PC[[2]]
	#rownames(X) <- rownames(PC$loadings) ###
	maximo <- apply(X,2,function(x){max(abs(x))})
	for(i in 1:2)  X[,i] <- X[,i]/maximo[i]
	#X <- X/max(abs(as.vector(X)))
	#vari <- PC$sdev^2                     ###
	vari <- PC[[1]]^2
	varexpl <- round(100*vari/sum(vari),2)
	xlab <- paste("PC 1 (",varexpl[1],"%)",sep="")
	ylab <- paste("PC 2 (",varexpl[2],"%)",sep="")
	par(cex=0.9)
	plot(0,0,xlim=c(-1,1)*1.25,ylim=c(-1,1)*1.25,pch="",xlab=xlab,ylab=ylab,main=main)
	par(cex=0.75)
  	for(i in 1:nrow(X)){
		arrows(0,0,X[i,1],X[i,2],code=2,length=0.1,col=2)
		text(X[i,1],X[i,2],rownames(X)[i],col='blue',pos=cuadrante2(X[i,]))
	}
	abline(h=0,lty=2)
	abline(v=0,lty=2)
}
