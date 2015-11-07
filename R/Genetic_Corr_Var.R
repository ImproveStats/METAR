
GenCorrVar <- function()
{
	cat("\n")
	cat(" NOTE: Phenotypes values are scaled to have mean zero and variance 1","\n")
	folderPlot <- "Dendograms&Biplots_Variables"
	if(!file.exists(folderPlot))  dir.create(folderPlot)
	if(typeAnalysis==3){
		nombre <- "Correlation_Variables_Individual_byManagement"
		manages <- selected
	}
	if(typeAnalysis==4){	
		nombre <- "Correlation_Variables_Individual_withoutManagement"
		manages <- "OVERALL"
		envirs <- selected
	}
	if(typeAnalysis==5){
		nombre <- "Correlation_Variables_Combined_byManagement"
		manages <- selected
		envirs <- "OVERALL"
	}	
	if(typeAnalysis==6){
		nombre <- "Correlation_Variables_Combined_acrossLocations"
		manages <- "OVERALL"
		envirs <- "OVERALL"
		index <- which(as.character(datos[,'Loc'])%in%selected)
		if(length(index)>0)	datos <- datos[index,]		
	}
	outGEN <- outPHEN <- outCOV <- c()
	flagNOTE <- FALSE
	for(man in 1:length(manages))
	{
		management <- manages[man]
		if(management=="OVERALL")	datos1 <- datos
		if(management!="OVERALL")	datos1 <- datos[as.character(datos[,"Manag"])%in%management,]

		if(typeAnalysis==3)	envirs <- unique(as.character(datos1[,"Loc"]))
  
		for(env in 1:length(envirs))
		{
			envir <- envirs[env]
			if(typeAnalysis==3) postfijo <- paste(management,". Location: ",envir,sep="")
			if(typeAnalysis==4) postfijo <- paste("Location: ",envir,sep="")
			if(typeAnalysis==5) postfijo <- management
			if(typeAnalysis==6) postfijo <- ""
			escribe_LOG("  ",FileName="LOG1.txt")
			escribe_LOG(paste(rep("-",87),collapse=""))
			escribe_LOG(postfijo)
			escribe_LOG(paste(rep("-",87),collapse=""))
			escribe_LOG("\tTrait\tGenotypic Variance\tHeritability",FileName="LOG1.txt")
			
			PlotFile <- paste0(nombre,ifelse(typeAnalysis%in%(3:4),paste0("_",envir),""),ifelse(typeAnalysis%in%c(3,5),paste0("_",management),""))
			PlotFile <- paste0(PlotFile,ifelse(!is.null(covariate),"_withCovariate_","_withoutCovariate_"),ExpDes)
	
			datos2 <- datos1[as.character(datos1[,"Loc"])%in%envir,]
			if(envir=="OVERALL")  datos2 <- datos1
  
			for(k in 1:length(var.class))  datos2[,var.class[k]] <- as.factor(as.character(datos2[,var.class[k]]))

			### Output matrices
			corGEN <- p.values <- matrix(NA,ncol=length(traits),nrow=length(traits))
			COV0 <- matrix("",ncol=length(traits),nrow=length(traits))
			dimnames(corGEN) <- dimnames(COV0) <- dimnames(p.values) <- list(traits,traits)
			
			BLUES <- matrix(NA,ncol=length(traits),nrow=length(unique(datos2[,'Entry'])))
			dimnames(BLUES) <- list(unique(as.character(datos2[,'Entry'])),traits)
			
			flag1 <- flag2 <- rep(TRUE,length(traits))
			
			for(i in 1:length(traits))
			{
				checa1 <- suppressWarnings(checa_datos(datos2,traits[i]))
				if(checa1$flag)
				{
				#==================== Var1 ====================#
				V1 <- suppressWarnings(as.numeric(as.character(datos2[,traits[i]])))
				V1 <- scale(V1)
				datos.tmp <- data.frame(datos2[,var.class],V1=V1)
				
				flagCOV <- FALSE
				if(!is.null(covariate))
				{	
					COV <- suppressWarnings(apply(matrix(as.matrix(datos2)[,covariate],ncol=length(covariate)),2,as.numeric))
					COV <- scale(COV)
					colnames(COV) <- paste("Cov",1:length(covariate),sep="")
					checaCOV <- list()
					for(k in 1:length(covariate)) checaCOV[[k]] <- suppressWarnings(checa_datos(datos2,covariate[k]))
					flagCOV2 <- unlist(lapply(checaCOV,function(x)x$flag))
					flagCOV <- prod(flagCOV2)==1
					if(flagCOV) datos.tmp <- data.frame(datos.tmp,COV)
				}

				if(ExpDes=="Lattice")
				{	
					fm.text <- ifelse(envir!="OVERALL","lmer(V1~(1|Entry)+(1|Rep)+(1|Block:Rep)",
						"lmer(V1~(1|Entry)+(1|Rep:Loc)+(1|Block:Rep:Loc)+(1|Entry:Loc)+(1|Loc)")
				}
				if(ExpDes=="RCB")
				{	
					fm.text <- ifelse(envir!="OVERALL","lmer(V1~(1|Entry)+(1|Rep)","lmer(V1~(1|Entry)+(1|Rep:Loc)+(1|Entry:Loc)+(1|Loc)")
				}
				tmp <- ifelse(flagCOV & !is.null(covariate), ifelse(adjALL,TRUE,ifelse(i==1,TRUE,FALSE)),FALSE)
				tmpCOV <- ifelse(tmp,paste("+Cov",1:length(covariate),collapse="",sep=""),"")
				fm.text <- paste(fm.text,tmpCOV,",data=datos.tmp)",sep="")
				fm <- suppressWarnings(eval(parse(text=fm.text)))

				varcorr <- VarCorr(fm)
				varU1 <- as.vector(varcorr$'Entry')
				varErr <- attr(varcorr,'sc')^2
				varGE <- as.vector(varcorr$'Entry:Loc')
				varLoc <- as.vector(varcorr$'Loc')
				tmp <- split(datos.tmp,as.character(datos.tmp[,'Loc']))
				nRep <- mean(unlist(lapply(tmp,function(x)length(unique(as.character(x[,'Rep']))))))
				nLoc <- length(unique(as.character(datos.tmp[,'Loc'])))
				
				h2 <- ifelse(envir=="OVERALL",varU1/(varU1 + varGE/nLoc + varErr/(nRep*nLoc)),varU1/(varU1 + varErr/nRep))
				if(round(h2,4)>0 & round(h2,4)>=limit.h2)  escribe_LOG(c("\t",rellena(traits[i]),"\t",round2(varU1,4),"\t",round2(h2,4),"\t\t"))
				if(round(h2,4)>0 & round(h2,4)<limit.h2){
				    flag2[i] <- FALSE
				    escribe_LOG(c("\t",rellena(traits[i]),"\t",round2(varU1,4),"\t",round2(h2,4),"\t\tHeritability smaller than threshold. Excluded*"))
				}
				if(round(h2,4)==0){
				    flag2[i] <- FALSE
				    escribe_LOG(c("\t",rellena(traits[i]),"\t",round2(varU1,4),"\t",round2(h2,4),"\t\tHeritability zero. Excluded*"))
				}

				# Compute BLUE (Entry as fixed effect)
				if(ExpDes=="Lattice")
				{	
					fm.text <- ifelse(envir!="OVERALL","lmer(V1~0+Entry+(1|Rep)+(1|Block:Rep)",
						"lmer(V1~0+Entry+(1|Rep:Loc)+(1|Block:Rep:Loc)+(1|Entry:Loc)+(1|Loc)")
				}
				if(ExpDes=="RCB")
				{	
					fm.text <- ifelse(envir!="OVERALL","lmer(V1~0+Entry+(1|Rep)","lmer(V1~0+Entry+(1|Rep:Loc)+(1|Entry:Loc)+(1|Loc)")
				}
				fm.text <- paste(fm.text,tmpCOV,",data=datos.tmp)",sep="")
				fm <- suppressWarnings(eval(parse(text=fm.text)))

				fixEffect <- fixef(fm)
				fixEffect <- fixEffect[grep("Entry",names(fixEffect))]
				names(fixEffect) <- substr(names(fixEffect),6,nchar(names(fixEffect)))
				BLUES[match(names(fixEffect),rownames(BLUES)),i] <- round(fixEffect,5)
				}
				if(!checa1$flag)
				{	flag1[i] <- FALSE
					escribe_LOG(c("\t",rellena(traits[i]),"\tNA\tNA\t\t",checa1$mensaje,". Excluded*"))
				}
				
				for(j in i:length(traits))
				{
					checa2 <- suppressWarnings(checa_datos(datos2,traits[j]))
					if(checa1$flag & checa2$flag)
					{
 					V2 <- suppressWarnings(as.numeric(as.character(datos2[,traits[j]]))) 
					V2 <- scale(V2)
					V3 <- V1+V2
	  
					#==================== Var2 ====================#
					datos.tmp <- data.frame(datos2[,var.class],V2=V2,V3=V3)
					if(flagCOV) datos.tmp <- data.frame(datos.tmp,COV)

					if(ExpDes=="Lattice")
					{	
						fm.text <- ifelse(envir!="OVERALL","lmer(V2~(1|Entry)+(1|Rep)+(1|Block:Rep)",
						"lmer(V2~(1|Entry)+(1|Rep:Loc)+(1|Block:Rep:Loc)+(1|Entry:Loc)+(1|Loc)")
					}
					if(ExpDes=="RCB")
					{	
						fm.text <- ifelse(envir!="OVERALL","lmer(V2~(1|Entry)+(1|Rep)","lmer(V2~(1|Entry)+(1|Rep:Loc)+(1|Entry:Loc)+(1|Loc)")
					}
					tmp <- ifelse(flagCOV & !is.null(covariate), ifelse(adjALL,TRUE,ifelse(j==1,TRUE,FALSE)),FALSE)
					tmpCOV <- ifelse(tmp,paste("+Cov",1:length(covariate),collapse="",sep=""),"")
					fm.text <- paste(fm.text,tmpCOV ,",data=datos.tmp)",sep="")
					fm <- suppressWarnings(eval(parse(text=fm.text)))

					varcorr <- VarCorr(fm)
					varU2 <- as.vector(varcorr$'Entry')
    
					#==================== Var3 ====================#
					if(ExpDes=="Lattice")
					{	
						fm.text <- ifelse(envir!="OVERALL","lmer(V3~(1|Entry)+(1|Rep)+(1|Block:Rep)",
						"lmer(V3~(1|Entry)+(1|Rep:Loc)+(1|Block:Rep:Loc)+(1|Entry:Loc)+(1|Loc)")
					}
					if(ExpDes=="RCB")
					{	
						fm.text <- ifelse(envir!="OVERALL","lmer(V3~(1|Entry)+(1|Rep)","lmer(V3~(1|Entry)+(1|Rep:Loc)+(1|Entry:Loc)+(1|Loc)")
					}
					fm.text <- paste(fm.text,tmpCOV ,",data=datos.tmp)",sep="")
					fm <- suppressWarnings(eval(parse(text=fm.text)))

					varcorr <- VarCorr(fm)
					varU3 <- as.vector(varcorr$'Entry')
    
					cov1.2 <- (varU3-varU1-varU2)/2 
					correGen <- round(cov1.2/(sqrt(varU1)*sqrt(varU2)),4)
					COV0[j,i] <- round(cov1.2,4)
					if(!is.na(correGen))
					{	if(correGen > 1) correGen <- 0.9999
						if(correGen < -1) correGen <- -0.9999
					}
					
					#==================== t test ====================#
					gl <- length(unique(as.character(datos.tmp[,'Entry'])))
					t0 <- abs(correGen/sqrt((1-correGen^2)/(gl-2)))
					p0 <- round(2*(1-pt(t0,gl-2)),4)
					if(!is.na(correGen)) if(p0 < 0.00001)  p0 <- 0.0001
					p.values[j,i] <- p0
					corGEN[j,i] <- correGen 					
					}
				}
			}
			flag <- flag1 & flag2
			flagNOTE <- ifelse(any(!flag),TRUE,flagNOTE)
			
			# If problems with covariates
			if(!flagCOV & !is.null(covariate)){
				escribe_LOG("",FileName="LOG1.txt")
				escribe_LOG("Warning:",FileName="LOG1.txt")
				for(k in 1:length(covariate)){
					if(!checaCOV[[k]]$flag) escribe_LOG(c("\t",checaCOV[[k]]$mensaje," in covariate '",covariate[k],"'"))
				}
				escribe_LOG(c("\t",ifelse(adjALL,"No variable was",paste(traits[1]," wasn't",sep=""))," adjusted by covariate"))
			}
			
			corPHEN <- round(cor(BLUES,use="pairwise.complete.obs"),4)
			corGEN[upper.tri(corGEN)] <- t(corGEN)[upper.tri(t(corGEN))]
			diag(corGEN) <- 1
			matrix.dist <- 1-corGEN[flag,flag]
      		
			# Plot
			if(BiplotFormat=="pdf") pdf(paste(folderPlot,"/",PlotFile,".pdf",sep=""),width=12.5,height=5.8)
			if(BiplotFormat=="wmf") win.metafile(paste(folderPlot,"/",PlotFile,".wmf",sep=""),width=12.5,height=5.8)
			if(BiplotFormat=="png") png(paste(folderPlot,"/",PlotFile,".png",sep=""),width=857.1,height=397.7)
			
			if(sum(flag)>2){
				par(mar=c(5.1,4.1,4.1,2.1))   
				layout(matrix(c(rep(1,12),rep(2,9)),ncol=7,nrow=3))
				hcl <- suppressMessages(hclust(as.dist(matrix.dist),"ward"))
				plot_dendrogram(hcl,main=paste("Dendrogram. ",postfijo,"\nWard method",sep=""),xlab="Trait")
				
				rownames(corGEN) <- rep("",nrow(corGEN))
				PC0 <- suppressWarnings(princomp(corGEN[flag,flag],cor=TRUE))
				biplot3(PC0,main=paste("Biplot. ",postfijo,sep=""))	
			}
			if(sum(flag)<=2){
    				par(mfrow=c(1,2),mar=c(5.1,4.1,4.1,2.1))
				plot(0,main="Dendrogram not available\nNo enough variables to plot",xlab="",ylab="",xaxt="n",yaxt="n",pch="")
				plot(0,main="Biplot not available\nNo enough variables to plot",xlab="",ylab="",xaxt="n",yaxt="n",pch="")
			}
			dev.off()
  
			### Save results
			corGEN[!flag,] <- corGEN[,!flag] <- NA
			p.values[!flag,] <- p.values[,!flag] <- NA
			corGEN[upper.tri(corGEN)] <- ""
			corPHEN[upper.tri(corPHEN)] <- ""
			p.values[upper.tri(p.values)] <- ""
			tmp <- cbind(c(postfijo,rep("",nrow(corGEN)-1)),Traits=traits,corGEN,rep("",nrow(corGEN)),p.values)
			outGEN <- rbind(outGEN,colnames(tmp),tmp,rep("",ncol(tmp)))
			tmp <- cbind(c(postfijo,rep("",nrow(corPHEN)-1)),Traits=traits,corPHEN)
			outPHEN <- rbind(outPHEN,colnames(tmp),tmp,rep("",ncol(tmp)))
			tmp <- cbind(c(postfijo,rep("",nrow(COV0)-1)),Traits=traits,COV0)
			outCOV <- rbind(outCOV,colnames(tmp),tmp,rep("",ncol(tmp)))
  		}
	}
	escribe_LOG("  ")	
	
	# Pone titulos a las matrices
	tmp <- c("","","Genetic Correlations",rep("",length(traits)),"Probabilities",rep("",length(traits)-1))
	if(length(outGEN)>0) outGEN <- rbind(tmp,rep("",2*length(traits)+3),outGEN)
	
	outfile <- paste0("Genetic_",nombre,ifelse(!is.null(covariate),"_withCovariate_","_withoutCovariate_"),ExpDes,".csv")
	write.table(outGEN,outfile,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",")
	
	outfile <- paste0("Phenotypic_",nombre,ifelse(!is.null(covariate),"_withCovariate_","_withoutCovariate_"),ExpDes,".csv")
	write.table(outPHEN,outfile,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",")
	
	tmp <- unlist(lapply(strsplit(nombre,"_"),function(x)paste(x[2:length(x)],collapse="_")))
	outfile <- paste0("Covariance_",tmp,ifelse(!is.null(covariate),"_withCovariate_","_withoutCovariate_"),ExpDes,".csv")
	write.table(outCOV,outfile,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",")
	
	if(flagNOTE){
	 cat("\n")
	 cat(" *Excluded. This trait was not taken into account in the computation of genetic correlation","\n")
	}
}

