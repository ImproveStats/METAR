round2 <-
function(numero,digits=0)
{ 
	tmp <- as.character(round(numero,digits)) 
	if(length(grep("\\.",tmp))>0){
		a0 <- unlist(strsplit(tmp,"\\."))[2]
		if(nchar(a0)<digits)	tmp <- paste(c(tmp,rep(0,digits-nchar(a0))),collapse="")
	}else{
		tmp <- paste(c(tmp,".",rep(0,digits)),collapse="")	
	}
	return(as.character(tmp))
}
