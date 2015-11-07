rm(list=ls())
options("scipen"=100, "digits"=8)

#====== Installing package
suppressWarnings(library(METAR,verbose=FALSE))


#====== Loading programs ...
source("tmp.R")
datos <- prepareData()

META(analysis)    
 
