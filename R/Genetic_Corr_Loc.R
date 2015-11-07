
GenCorrLoc <- function()
{
	cat("\n")
	cat(" NOTE: Phenotypes values are scaled to have mean zero and variance 1","\n")
	folderPlot <- "Dendograms&Biplots_Locations"
	if(!file.exists(folderPlot))  dir.create(folderPlot)
	if(typeAnalysis==1){
		nombre <- "Correlation_Locations_byManagement"
		manages <- selected
	}
	if(typeAnalysis==2){
		nombre <- "Correlation_Locations_withoutManagement"
		manages <- "OVERALL"
		index <- which(as.character(datos[,'Loc'])%in%selected)
		if(length(index)>0)	datos <- datos[index,]		
	}
	flagNOTE <- FALSE
	for(man in 1:length(manages))
	{
		management <- manages[man]
		if(management=="OVERALL")	datos1 <- datos
		if(management!="OVERALL")	datos1 <- datos[as.character(datos[,"Manag"])%in%management,]
		
		outGEN <- outPHEN <- c()		
		for(i in 1:length(traits))
		{
			if(typeAnalysis==1) postfijo <- paste0(management,". Trait: ",traits[i])
			if(typeAnalysis==2) postfijo <- paste0("Trait: ",traits[i])	
		
			escribe_LOG("  ",FileName="LOG1.txt")
			escribe_LOG(paste(rep("-",87),collapse=""))
			escribe_LOG(postfijo)
			escribe_LOG(paste(rep("-",87),collapse=""))
			escribe_LOG("\tLocation\tGenotypic Variance\tHeritability",FileName="LOG1.txt")
      
			#Plot
			PlotFile <- paste0(nombre,ifelse(typeAnalysis==1,paste0("_",management),""))
			PlotFile <- paste0(PlotFile,ifelse(!is.null(covariate),"_withCovariate_","_withoutCovariate_"),ExpDes,"_",traits[i])
			
			envirs <- unique(as.character(datos1[,"Loc"]))
			h2 <- rep(NA,length(envirs))
			names(h2) <- envirs
			flag1 <- flag2 <- rep(TRUE,length(envirs))

			Entries <- unique(as.character(datos1[,'Entry']))
			Entries <- Entries[order(Entries)]
			BLUE.matrix <- matrix(NA,nrow=length(unique(as.character(datos1[,'Entry']))),ncol=length(envirs))
			dimnames(BLUE.matrix) <- list(Entries,envirs)
			for(env in 1:length(envirs))
			{
				datos2 <- datos1[as.character(datos1[,"Loc"])%in%envirs[env],]
				for(k in 1:length(var.class))  datos2[,var.class[k]] <- as.factor(as.character(datos2[,var.class[k]]))

				checa1 <- suppressWarnings(checa_datos(datos2,traits[i]))
				if(checa1$flag)
				{
					y <- suppressWarnings(as.numeric(as.character(datos2[,traits[i]])))
					y_est <- (y-mean(y,na.rm=TRUE))/sd(y,na.rm=TRUE)

					datos.tmp <- data.frame(datos2[,var.class],y,y_est)
					flagCOV <- FALSE
					if(!is.null(covariate))
					{	
						COV <- suppressWarnings(apply(matrix(as.matrix(datos2)[,covariate],ncol=length(covariate)),2,as.numeric))
						COV <- scale(COV)
						colnames(COV) <- paste0("Cov",1:length(covariate))
						checaCOV <- list()
						for(k in 1:length(covariate)) checaCOV[[k]] <- suppressWarnings(checa_datos(datos2,covariate[k]))
						flagCOV2 <- unlist(lapply(checaCOV,function(x)x$flag))
						flagCOV <- prod(flagCOV2)==1
						if(flagCOV) datos.tmp <- data.frame(datos.tmp,COV)
					}
          tmp <- ifelse(flagCOV & !is.null(covariate), ifelse(adjALL,TRUE,ifelse(i==1,TRUE,FALSE)),FALSE)
					tmpCOV <- ifelse(tmp,paste("+Cov",1:length(covariate),collapse="",sep=""),"")
					fm.text <- ifelse(ExpDes=="RCB","lmer(y_est~(1|Entry)+(1|Rep)","lmer(y_est~(1|Entry)+(1|Rep)+(1|Block:Rep)")										
					fm.text <- paste(fm.text,tmpCOV,",data=datos.tmp)",sep="")
					fm <- suppressWarnings(eval(parse(text=fm.text)))

					#  Compute h2
					varcorr <- VarCorr(fm)
					varG <- as.vector(varcorr$'Entry')
					varErr <- attr(varcorr,'sc')^2
					nRep <- length(unique(as.character(datos.tmp[,'Rep'])))
					h2[env] <- varG/(varG + varErr/nRep)
				
					tmp <- ifelse(!flagCOV & !is.null(covariate), ifelse(adjALL,TRUE,ifelse(i==1,TRUE,FALSE)),FALSE)
					extra <- ifelse(tmp,paste("Problems in covariate ",paste(covariate[!flagCOV2],collapse=","),". Not adjusted by covariate",sep=""),"")
					if(round(h2[env],4)>0 & round(h2[env],4)>=limit.h2)  escribe_LOG(c("\t",rellena(envirs[env]),"\t",round2(varG,4),"\t",round2(h2[env],4),"\t\t",extra))
					if(round(h2[env],4)>0 & round(h2[env],4)< limit.h2){
						flag2[env] <- FALSE
				  		escribe_LOG(c("\t",rellena(envirs[env]),"\t",round2(varG,4),"\t",round2(h2[env],4),"\t\tHeritability smaller than threshold. Excluded*"))   
					}
					if(round(h2[env],4)==0){
						flag2[env] <- FALSE
				  		escribe_LOG(c("\t",rellena(envirs[env]),"\t",round2(varG,4),"\t",round2(h2[env],4),"\t\tHeritability zero. Excluded*"))   
					}

					#BLUEs for Phenotypic correlations
					fm.text <- ifelse(ExpDes=="RCB","lmer(y~0+Entry+(1|Rep)","lmer(y~0+Entry+(1|Rep)+(1|Block:Rep)")					
					fm.text <- paste(fm.text,tmpCOV,",data=datos.tmp)",sep="")
					fm <- suppressWarnings(eval(parse(text=fm.text)))
					fixEffect <- fixef(fm)
					fixEffect <- fixEffect[grep("Entry",names(fixEffect))]
					tmp <- substr(names(fixEffect),6,nchar(names(fixEffect)))
					BLUE.matrix[match(tmp,Entries),env] <- round(fixEffect,5)
					#yAverage <- tapply(FUN=mean,X=datos.tmp[,'y'],INDEX=as.character(datos.tmp[,'Entry']),na.rm=TRUE)
					#BLUE.matrix[match(names(yAverage),Entries),env] <- round(yAverage,5)
	
				}
				if(!checa1$flag)
				{	flag1[env] <- FALSE
					escribe_LOG(c("\t",rellena(envirs[env]),"\tNA\tNA\t\t",checa1$mensaje,". Excluded*"))
				}	
			}
		
			corPHEN <- cor(BLUE.matrix,use="pairwise.complete.obs")
			flag <- flag1 & flag2
			flagNOTE <- ifelse(any(!flag),TRUE,flagNOTE)
		      
			# Genetic correlations
			corGEN <- matrix(NA,ncol=length(envirs),nrow=length(envirs))
			dimnames(corGEN) <- list(colnames(corPHEN),colnames(corPHEN))
			for(j in 1:length(envirs))
			{
				for(k in j:length(envirs))
				{ corGEN[j,k] <- corGEN[k,j] <- round(corPHEN[j,k]/sqrt(h2[j]*h2[k]),4)
				}
			}
			diag(corGEN) <- 1
			corGEN[corGEN > 1] <- .9999
			corGEN[corGEN < -1] <- -0.9999

			matrix.dist <- 1-corGEN[flag,flag]
		  
			if(BiplotFormat=="pdf") pdf(paste(folderPlot,"/",PlotFile,".pdf",sep=""),width=12.5,height=5.8)
			if(BiplotFormat=="wmf") win.metafile(paste(folderPlot,"/",PlotFile,".wmf",sep=""),width=12.5,height=5.8)
			if(BiplotFormat=="png") png(paste(folderPlot,"/",PlotFile,".png",sep=""),width=857.1,height=397.7)
			
			if(sum(flag)>2){
				par(mar=c(5.1,4.1,4.1,2.1)) 
				layout(matrix(c(rep(1,12),rep(2,9)),ncol=7,nrow=3))
				hcl <- suppressMessages(hclust(as.dist(matrix.dist),"ward"))
				plot_dendrogram(hcl,main=paste("Dendrogram. ",postfijo,"\nWard method",sep=""),xlab="Location")
					
				rownames(corGEN) <- rep("",nrow(corGEN))
				PC0 <- suppressWarnings(prcomp(corGEN[flag,flag],cor=T,scale=T,retx = T))
				biplot3(PC0,main=paste("Biplot. ",postfijo,sep=""))	
			}
			if(sum(flag)<=2){
				par(mfrow=c(1,2),mar=c(5.1,4.1,4.1,2.1))
				plot(0,main="Dendrogram not available\nNo enough locations to plot",xlab="",ylab="",xaxt="n",yaxt="n",pch="")
				plot(0,main="Biplot not available\nNo enough locations to plot",xlab="",ylab="",xaxt="n",yaxt="n",pch="")
			}
			dev.off()
			
			rownames(corGEN) <- colnames(corGEN)
			corGEN[!flag,] <- corGEN[,!flag] <- NA
			corGEN[upper.tri(corGEN)] <- ""
			corPHEN <- round(corPHEN,4)
			corPHEN[upper.tri(corPHEN)] <- ""
			tmp <- cbind(c("",postfijo,rep("",nrow(corGEN)-1)),c("Loc",rownames(corGEN)),rbind(colnames(corGEN),corGEN))
			outGEN <- rbind(outGEN,tmp,rep("",ncol(tmp)))	
			
			tmp <- cbind(c("",postfijo,rep("",nrow(corPHEN)-1)),c("Loc",rownames(corPHEN)),rbind(colnames(corPHEN),corPHEN))
			outPHEN <- rbind(outPHEN,tmp,rep("",ncol(tmp)))	
 		}
		outfile <- paste0("Genetic_",nombre,ifelse(typeAnalysis==1,paste0("_",management),""))
		outfile <- paste0(outfile,ifelse(!is.null(covariate),"_withCovariate_","_withoutCovariate_"),ExpDes,".csv")
		write.table(outGEN,outfile,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",")
		
		outfile <- paste0("Phenotypic_",nombre,ifelse(typeAnalysis==1,paste0("_",management),""))
		outfile <- paste0(outfile,ifelse(!is.null(covariate),"_withCovariate_","_withoutCovariate_"),ExpDes,".csv")
		write.table(outPHEN,outfile,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",")
	}
	escribe_LOG("  ")
	
	if(flagNOTE){
	 cat("\n")
	 cat(" *Excluded. This location was not taken into account in the computation of genetic correlation","\n")
	}
}

