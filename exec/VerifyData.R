rm(list=ls())
options("scipen"=100, "digits"=8)

library(METAR)
source("tmp.R")

var.class <- cambia_caracter(var.class)
traits <- cambia_caracter(traits)

datos <- read.csv(paste(dir_file,"/",file_name,sep=""),header=TRUE,sep=",")
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
datos[,"Loc"] <- cambia_caracter(as.character(datos[,"Loc"]))

hayManejo <- FALSE
manages <- "OVERALL"
if("Manag"%in%colnames(datos)){
	datos[,"Manag"] <- cambia_caracter(as.character(datos[,"Manag"]))
	manages <- unique(as.character(datos[,"Manag"]))
	hayManejo <- TRUE
}

setwd(dir_root)
if(!file.exists("Output")) dir.create("Output")
setwd("Output")
if(!file.exists(outFolder)) dir.create(outFolder)
setwd(outFolder)

np <- 0
BASIC_STATS()
escribe_LOG(paste(" Results of the Descriptive Analysis of  '",file_name,"'",sep=""))
for(man in 1:length(manages))
{
	management <- manages[man]
	if(!hayManejo)	datos1 <- datos
	if(hayManejo)	datos1 <- datos[as.character(datos[,'Manag'])%in%management,]
		
	envirs <- unique(as.character(datos1[,"Loc"]))
	for(env in 1:length(envirs))
	{
		envir <- envirs[env]
		combi <- paste0(" - ",ifelse(hayManejo,paste0("Management: ",management,". Location: "),"Location: "),envir) 
		datos2 <- datos1[as.character(datos1[,"Loc"])%in%envir,]
		cont <- 0
		for(i in 1:length(traits))
		{
			checa <- suppressWarnings(checa_datos(datos2,traits[i]))
			if(checa$mensaje!="OK"){
				np <- np + 1
				if(np==1){
					escribe_LOG("")
					escribe_LOG(" Some problems were found in data...")
				}
				if(cont==0){
					escribe_LOG("")
					escribe_LOG(combi)
				}
				escribe_LOG(paste0("     ",traits[i],": ",checa$mensaje))
				cont <- cont + 1
			}
		}
	}
}
escribe_LOG("")
if(box_plot)            BOXPLOTS()
if(histogramas)    HISTOGRAMAS()

