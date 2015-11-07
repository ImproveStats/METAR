
prepareData <- function()
{
	var.class <- cambia_caracter(var.class)
	traits <- cambia_caracter(traits)
	if(!is.null(covariate)) covariate <- cambia_caracter(covariate)
	outFolder <- cambia_caracter(outFolder)
	selected <- cambia_caracter(selected)

	#====== Reading data ...
	datos <- read.csv(paste(dir_file,"/",file_name,sep=""),sep=",",header=TRUE)
	colNames <- scan(paste(dir_file,"/",file_name,sep=""),what="character",sep=",",nlines=1,quiet=T)
	colnames(datos) <- cambia_caracter(quita_espacio(colNames))

	indexVAR <- !var.class%in%c("none")
	new.names <- new.names[indexVAR]; var.class <- var.class[indexVAR]
	match0 <- match(var.class,colnames(datos))
	colnames(datos)[match0] <- new.names
	match1 <- which(colnames(datos)%in%new.names)
	indexDROP <- match1[!match1%in%match0]
	if(length(indexDROP)>0) datos <- datos[,-indexDROP]
	var.class <- new.names
	datos <- datos[,unique(c(var.class,covariate,traits))]
	datos[,"Loc"] <- cambia_caracter(as.character(datos[,"Loc"]))
	if("Manag"%in%colnames(datos))	datos[,"Manag"] <- cambia_caracter(as.character(datos[,"Manag"]))

	setwd(dir_root)
	if(!file.exists("Output")) dir.create("Output")
	setwd("Output")
	if(!file.exists(outFolder)) dir.create(outFolder)
	setwd(outFolder)
	
	var.class <<- var.class
	traits <<- traits
	covariate <<- covariate
	outFolder <<- outFolder
	selected <<- selected
	var.class <<- var.class

	return(datos)
}

META <- function(analysis)
{
	cat("\n")
	cat("=======================","\n")
	cat("        SUMMARY OF RESULTS","\n")
	cat("=======================","\n")
	if(analysis==1)    GenCorrLoc()
	if(analysis==2)    GenCorrVar()
	if(analysis==3)    BLUE_BLUP()
}



