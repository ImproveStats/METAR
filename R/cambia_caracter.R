cambia_caracter <-
function(cadena)
{
 	especiales <- "[]=%$+,;<>:/|*?~"
	tmp <- unlist(lapply(strsplit(cadena,""),function(x){
		for(i in 1:length(x))	x[i] <- ifelse(length(grep(x[i],especiales))>0,"_",x[i])
		return(paste(x,collapse=""))
	}))
	return(tmp)
}
