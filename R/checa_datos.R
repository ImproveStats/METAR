checa_datos <-
function(datos,y)
{
	# datos <- datos2 ; y <- traits[i]
	flag <- TRUE
	error <- 0
	mensaje <- "OK"
	datos <- data.frame(datos[,c("Entry","Loc","Rep",y)])
	colnames(datos) <- c("Entry","Loc","Rep",y)
	datos0 <- split(datos,as.character(datos[,"Loc"]))
	calculo <- lapply(datos0,function(x)
	{
		flag <- TRUE
		error <- 0
		mensaje <- "OK"
					
			dd <- split(x,as.character(x[,"Rep"]))
			nRep <- length(dd)
			ee <- lapply(dd,function(xx){
				y0 <- as.numeric(as.character(xx[,y]))
				ID <- as.character(xx[,"Entry"])
				return(data.frame(ID,y0))
			})
			ee0 <- data.frame(ee[[1]])
			if(length(ee)>1) for(ii in 2:length(ee)) ee0 <- merge(ee0,data.frame(ee[[ii]]),by="ID")
			
			# Suma valores no nulos de toda la replica
			sumas <- unlist(lapply(dd,function(xx){
				return(sum(!is.na(as.numeric(as.character(xx[,y])))))
			}))
			SD <- apply(matrix(as.matrix(ee0[,-1]),ncol=ncol(ee0)-1,nrow=nrow(ee0)),2,sd,na.rm=TRUE)
			SD <- SD[!is.na(SD)]
			corre <- c()
			if(nrow(ee0)>0)
			{	corre <- cor(matrix(as.matrix(ee0[,-1]),ncol=ncol(ee0)-1,nrow=nrow(ee0)),use="pairwise.complete.obs")
				corre <- corre[lower.tri(corre)]
				corre <- round(corre[!is.na(corre)],3)
			}
			if(nrow(ee0)==0) nRep <- 1
			if(length(corre)>0 & any(corre==1) & sum(sumas==0)==0){
				flag <- FALSE
				error <- 1   # correlated
				mensaje <- "Some replicates are correlated"
			}
			if(length(corre)>0 & any(corre==1) & any(sumas==0)){
				flag <- FALSE
				error <- 2   # correlated and null
				mensaje <- "Some replicates are correlated and some others unscored"
			}
			if(any(sumas==0) & length(corre)==0){
				flag <- FALSE
				error <- 3  # null
				mensaje <- "Some replicates are unscored"
			}
			if(sum(sumas)==0 & length(corre)==0){
				flag <- FALSE
				error <- 4  # null
				mensaje <- "All replicates are unscored"
			}
			if(nRep==1){
				flag <- FALSE
				error <- 5  # no replicated
				mensaje <- "Genotypes not replicated"
			}
			if(length(SD)>0 & sum(SD==0)>1){
				flag <- FALSE
				error <- 6  # valores unicos
				mensaje <- "There are no variance in any replicate"
			}
		return(list(flag=flag,error=error,mensaje=mensaje))
	})

	if(length(calculo)>1)
	{
		tmp <- unlist(lapply(calculo,function(x)x[1]))
		tmp2 <- unlist(lapply(calculo,function(x)x[2]))!=4
		if(sum(tmp2)<2)	## Any with error  4(All records null)
		{
			flag <- FALSE
			mensaje <- paste(sum(!tmp)," out of ",length(tmp)," are unscored locations",sep="")
			error <- 7
		}
		return(list(flag=flag,error=error,mensaje=mensaje))
	}
	if(length(calculo)==1) return(calculo[[1]])
}
