escribe_LOG <-
function(linea,FileName="LOG1.txt",consola=TRUE)
{
	FilePath <- paste0(getDirRoot(),"/exec/",FileName)
	write.table(paste0(paste(linea,collapse="")),FilePath,append=TRUE,row.names=FALSE,col.names=FALSE,quote=FALSE)
	if(consola) cat(paste(paste(linea,collapse=""),"\n",sep=""))
}
