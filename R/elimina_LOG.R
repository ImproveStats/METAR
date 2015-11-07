elimina_LOG <-
function(FileName="LOG1.txt")
{
	FilePath <- paste0(getDirRoot(),"/exec/",FileName)
	if(file.exists(FilePath)) file.remove(FilePath)
}
