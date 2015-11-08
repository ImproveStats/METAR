METAR <- function()
{ 
	tmp <- getwd()
	setwd(getDirRoot())
	system("java -jar exec/META-R.jar &>exec/LOG_METAR.txt &")
	setwd(tmp)
}

getDirRoot <- function()
{
    dirlib <- .libPaths()
    if (length(dirlib) > 0) {
        for (i in 1:length(dirlib)) {
            tmp <- paste0(dirlib[i],"/METAR")
            if(file.exists(tmp)) 
                folder <- paste0(dirlib[i],"/METAR")
        }
    }
    return(folder)
}
