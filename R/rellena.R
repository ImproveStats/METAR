rellena <-
function(cadena,n=17)
{ 
  tmp <- n-nchar(cadena)
  if(nchar(cadena)<n)  out <- paste(c(cadena,rep(" ",ifelse(nchar(cadena)<7,tmp+6,tmp+2))),collapse="")
  #if(nchar(cadena)<n)  out <- paste(c(cadena,rep(" ",tmp+2)),collapse="")
  if(nchar(cadena)>=n)   out <- paste(substr(cadena,1,n-1),"...",sep="")
  out
}
