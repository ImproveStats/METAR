get_h2 <-
function(datos=datos2,traits=traits,whichTrait=i,covariate=covariate)
{      
	envirs <- unique(as.character(datos[,"Loc"]))
	h2 <- rep(NA,length(envirs))
	names(h2) <- envirs

	datos.tmp <- split(datos,as.character(datos[,'Loc']))
	tmp <- lapply(datos.tmp,function(x){
		for(k in 1:length(var.class))  x[,var.class[k]] <- as.factor(as.character(x[,var.class[k]]))	
		out <- rep(NA,5)
		checa <- suppressWarnings(checa_datos(x,traits[whichTrait]))
		if(checa$flag)
		{
			tt <- suppressWarnings(data.frame(x[,var.class],y=as.numeric(as.character(x[,traits[whichTrait]])))) 
			tt <- data.frame(tt,y_est=scale(tt$y))
			flagCOV <- FALSE
			if(!is.null(covariate))
			{	
				COV <- suppressWarnings(apply(matrix(as.matrix(x)[,covariate],ncol=length(covariate)),2,as.numeric))
				COV <- scale(COV)
				colnames(COV) <- paste("Cov",1:length(covariate),sep="")
				checaCOV <- list()
				for(k in 1:length(covariate)) checaCOV[[k]] <- suppressWarnings(checa_datos(x,covariate[k]))
				flagCOV2 <- unlist(lapply(checaCOV,function(x)x$flag))
				flagCOV <- prod(flagCOV2)==1
				if(flagCOV) tt <- data.frame(tt,COV)
			}

			fm.text <- ifelse(ExpDes=="RCB","lmer(y~(1|Entry)+(1|Rep)","lmer(y~(1|Entry)+(1|Rep)+(1|Block:Rep)")						
			tmpCOV <- ifelse(flagCOV & !is.null(covariate) & whichTrait==1,paste("+Cov",1:length(covariate),collapse="",sep=""),"")
			fm.text <- paste(fm.text,tmpCOV,",data=tt)",sep="")
			fm <- suppressWarnings(eval(parse(text=fm.text)))

			#  Compute h2
			varcorr <- VarCorr(fm)
			varG <- as.vector(varcorr$'Entry')
			varErr <- attr(varcorr,'sc')^2
			nRep <- length(unique(as.character(x[,'Rep'])))
			h2 <- round(varG/(varG + varErr/nRep),3)
			out[1:5] <- c(as.character(unique(x[,'Loc'])),round(varG,3),round(varErr,3),h2,"")
		}
		if(!checa$flag) out[c(1,5)] <- c(as.character(unique(x[,'Loc'])),checa$mensaje)
		return(out)		
	})		
	out <- do.call('rbind',tmp)
	colnames(out) <- c("Location","Genotype Variance","Residual Variance","Heritability","")	
	return(out)	
}
