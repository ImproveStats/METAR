rm(list=ls())

library(METAR)


escribe_LOG(paste("\tR version:\t\t\t\t",version$version.string,sep=''),FileName="LOG0.txt") 
a <- installed.packages()
tmp <- "lme4"%in%rownames(a)
if(tmp) library(lme4)
version1 <- a[rownames(a)=='lme4',"Version"]
escribe_LOG(paste("\tPackage 'lme4':\t\t\t\t",ifelse(tmp,"","NO "),"INSTALLED",ifelse(tmp,paste(" (Version ",version1,")",sep=""),""),sep=''),FileName="LOG0.txt") 

tmp <- "lsmeans"%in%rownames(a)
if(tmp) library(lsmeans)
version1 <- a[rownames(a)=='lsmeans',"Version"]
escribe_LOG(paste("\tPackage 'lsmeans':\t\t\t\t",ifelse(tmp,"","NO "),"INSTALLED",ifelse(tmp,paste(" (Version ",version1,")",sep=""),""),sep=''),FileName="LOG0.txt") 
