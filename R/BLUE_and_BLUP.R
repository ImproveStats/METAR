
BLUE_BLUP <- function()
{
	if(typeAnalysis==3){
		nombre <- "BLUPs_Individual_byManagement"
		manages <- selected
	}
	if(typeAnalysis==4){			
		nombre <- "BLUPs_Individual_withoutManagement"
		manages <- "OVERALL"
		envirs <- selected
	}
	if(typeAnalysis==5){
		nombre <- "BLUPs_Combined_byManagement"
		manages <- selected
		envirs <- "OVERALL"
	}	
	if(typeAnalysis==6){
		nombre <- "BLUPs_Combined_acrossLocations"
		manages <- "OVERALL"
		envirs <- "OVERALL"
		index <- which(as.character(datos[,"Loc"])%in%selected)
		if(length(index)>0)	datos <- datos[index,]
	}
	outH2 <- c() 
	for(man in 1:length(manages))
	{
		management <- manages[man]
		if(management=="OVERALL")	datos1 <- datos
		if(management!="OVERALL")	datos1 <- datos[as.character(datos[,'Manag'])%in%management,]  
		if(typeAnalysis==3)	envirs <- unique(as.character(datos1[,"Loc"]))		
		outMATRIX <- c()
		for(env in 1:length(envirs))
		{
			envir <- envirs[env]
			if(typeAnalysis==3)	postfijo <- paste(management,". Location: ",envir,sep="")
			if(typeAnalysis==4)	postfijo <- paste("Location: ",envir,sep="")
			if(typeAnalysis==5)	postfijo <- management
			if(typeAnalysis==6)	postfijo <- ""

			escribe_LOG("  ",FileName="LOG1.txt")
			escribe_LOG(paste(rep("-",107),collapse=""))
			escribe_LOG(postfijo,FileName="LOG1.txt")
			escribe_LOG(paste(rep("-",107),collapse=""))
			escribe_LOG("\tTrait\tGenotypic Variance\tHeritability\tp Value")
			
			datos2 <- datos1[as.character(datos1[,"Loc"])%in%envir,]
			STATout <- matrix("",nrow=8,ncol=length(traits)+1)
			STATout[,1] <- c("Heritability","Genotype Variance","Residual Variance","Grand Mean","LSD","CV","n Replicates","Genotype significance")
			
			if(envir=="OVERALL")
			{	
				datos2 <- datos1
				STATout <- matrix("",nrow=11,ncol=length(traits)+1)
				STATout[,1] <- c("Heritability","Location Variance","Genotype Variance","GenxLoc Variance","Residual Variance",
					"Grand Mean","LSD","CV","n Replicates","n Locations","Genotype significance")
			}

			if(!is.null(covariate))
			{
				tmp <- matrix("",ncol=length(traits)+1,nrow=length(covariate))
				tmp[,1] <- paste("Covariate signif (",covariate,")",sep="")
				STATout <- rbind(STATout,tmp)
			}
			colnames(STATout) <- c("Statistic",paste("BLUE_",traits,sep=""))			
			for(k in 1:length(var.class))  datos2[,var.class[k]] <- as.factor(as.character(datos2[,var.class[k]])) 					
			
			results <- matrix(NA,ncol=2*length(traits)+1,nrow=length(unique(datos2[,"Entry"])))
			colnames(results) <- c("Genotype",paste(rep(c("BLUE","BLUP"),length(traits)),rep(traits,each=2),sep="_"))
			
			Entries <- unique(as.character(datos2[,"Entry"]))
			Entries <- Entries[order(Entries)]
			results[,1] <- Entries
			
			for(i in 1:length(traits))
			{
				flagCOV <- FALSE
				if(!is.null(covariate))
				{	
					COV <- suppressWarnings(apply(matrix(as.matrix(datos2)[,covariate],ncol=length(covariate)),2,as.numeric))
					COV <- scale(COV)
					colnames(COV) <- paste("Cov",1:length(covariate),sep="")
					checaCOV <- list()
					for(k in 1:length(covariate)) checaCOV[[k]] <- suppressWarnings(checa_datos(datos2,covariate[k]))
					flagCOV <- sum(unlist(lapply(checaCOV,function(x)x$flag)))==length(covariate)
				}
				datos.tmp <- suppressWarnings(data.frame(datos2[,var.class],y=as.numeric(as.character(datos2[,traits[i]])))) 
				flagComb <- TRUE; toComb <- NULL
				if(envir=="OVERALL")
				{
					novacias <- unlist(lapply(split(datos.tmp,as.character(datos.tmp[,"Loc"])),function(x)any(!is.na(x[,"y"]))))
					tt <- get_h2(datos=datos2,traits=traits,whichTrait=i,covariate=covariate)
					outH2 <- rbind(outH2,cbind(c(ifelse(postfijo=="",traits[i],paste(postfijo,". ",traits[i],sep="")),rep("",nrow(tt))),rbind("",tt)),"")
					h20 <- as.numeric(as.character(tt[,'Heritability']))
					if(limit.h2> -1) toComb <- rownames(tt)[h20>limit.h2 & !is.na(h20)]
					if(limit.h2 == -1) toComb <- names(novacias[novacias])
					if(length(toComb)<2) flagComb <- FALSE 
					index <- as.character(datos.tmp[,"Loc"])%in%toComb
					datos.tmp <- datos.tmp[index,]
					if(flagCOV){
						COV <- matrix(COV[index,],ncol=length(covariate))
						colnames(COV) <- paste("Cov",1:length(covariate),sep="")
					}
				}
				if(flagCOV) datos.tmp <- data.frame(datos.tmp,COV)

				#head(datos.tmp)
				checa <- suppressWarnings(checa_datos(datos2,traits[i]))
				if(checa$flag & flagComb)
				{   
				tmp <- ifelse(flagCOV & !is.null(covariate), ifelse(adjALL,TRUE,ifelse(i==1,TRUE,FALSE)),FALSE)
				tmpCOV <- ifelse(tmp,paste("+Cov",1:length(covariate),collapse="",sep=""),"")				
				model0 <- paste(ExpDes,ifelse(envir=="OVERALL","Comb","Ind"),ifelse(envir=="OVERALL",modelo,""),sep="&&")
				#  Compute BLUE (Entry as fixed effect)
				fm.text <- switch(model0,
					'RCB&&Ind&&'            =   "lm(y~0+Entry+Rep", # This is the second one
					'Lattice&&Ind&&'        = "lmer(y~0+Entry+Rep+(1|Block:Rep)",  # this is the first one
					'RCB&&Comb&&fixed'      =   "lm(y~0+Entry+Rep:Loc+Entry:Loc+Loc",
					'RCB&&Comb&&random'     = "lmer(y~0+Entry+(1|Rep:Loc)+(1|Entry:Loc)+(1|Loc)",
					'Lattice&&Comb&&fixed'  = "lmer(y~0+Entry+Rep:Loc+(1|Block:Rep:Loc)+Entry:Loc+Loc",
					'Lattice&&Comb&&random' = "lmer(y~0+Entry+(1|Rep:Loc)+(1|Block:Rep:Loc)+(1|Entry:Loc)+(1|Loc)"
				)
				fm.text <- paste(fm.text,tmpCOV,",data=datos.tmp)",sep="")
				fm <- suppressWarnings(eval(parse(text=fm.text)))					
				fixEffect <- summary(lsmeans(fm, ~Entry, 'revpairwise'))[,1:2]			
				results[match(as.character(fixEffect[,"Entry"]), Entries),2*(i-1)+2] <- round(fixEffect[,2],3)  
				
				#  Compute BLUP   (Entry as random effect)
				fm.text <- switch(model0,
					'RCB&&Ind&&'            = "lmer(y~(1|Entry)+Rep",  #This is the second one
					'Lattice&&Ind&&'        = "lmer(y~(1|Entry)+Rep+(1|Block:Rep)",    # This is the first one
					'RCB&&Comb&&fixed'      = "lmer(y~(1|Entry)+Rep:Loc+(1|Entry:Loc)+Loc",
					'RCB&&Comb&&random'     = "lmer(y~(1|Entry)+(1|Rep:Loc)+(1|Entry:Loc)+(1|Loc)",
					'Lattice&&Comb&&fixed'  = "lmer(y~(1|Entry)+Rep:Loc+(1|Block:Rep:Loc)+(1|Entry:Loc)+Loc",
					'Lattice&&Comb&&random' = "lmer(y~(1|Entry)+(1|Rep:Loc)+(1|Block:Rep:Loc)+(1|Entry:Loc)+(1|Loc)"
				)                            															
				fm.text <- paste(fm.text,tmpCOV,",data=datos.tmp)",sep="")
				fm2 <- suppressWarnings(eval(parse(text=fm.text)))									                				
				fixEffectI <- as.matrix(fixef(fm2))
				randEffect <- as.matrix(ranef(fm2))
				media <- mean(fixEffect$lsmean, na.rm=TRUE)
				EntryEffect <- randEffect[["Entry",1]]
				BLUP <- media+EntryEffect[,1]
				results[match(rownames(EntryEffect),Entries),2*(i-1)+3] <- round(BLUP,3)   	
				
				#  Compute statistics
				varcorr <- VarCorr(fm2)
				varErr <- attr(varcorr,'sc')^2
				varG <- as.vector(varcorr$'Entry')
				varGE <- as.vector(varcorr$'Entry:Loc')
				varLoc <- ifelse(is.null(varcorr$'Loc'),NA,as.vector(varcorr$'Loc'))
				tmp <- split(datos.tmp,as.character(datos.tmp[,"Loc"]))
				nRep <- mean(unlist(lapply(tmp,function(x)length(unique(as.character(x[,"Rep"]))))))
				nGen <- mean(unlist(lapply(tmp,function(x)length(unique(as.character(x[,"Entry"]))))))
				nBlock <- ifelse(ExpDes=="Lattice",mean(unlist(lapply(tmp,function(x)length(unique(as.character(x[,"Block"])))))),NA)
				nLoc <- length(unique(as.character(datos.tmp[,"Loc"])))
                	 											 										 
				# Calculate h2 and fill the table of statistics
				#SED <- suppressMessages(summary((contrast(lsmeans(fm,~Entry),'revpairwise'))))
				SED <- suppressMessages(summary((contrast(lsmeans(fm,~Entry)))))
				CV <- 100*mean(SED$SE, na.rm=TRUE)/abs(media)
				if(envir!="OVERALL"){
					gl0 <- ifelse(ExpDes=="Lattice",nGen*(nRep-1)-nBlock+1,(nRep-1)*(nGen-1))
					LSD <- mean(SED$SE, na.rm=TRUE)*qt(1-0.05/2, round(gl0))
					tmp <- c("Entry",grep("Cov",rownames(anova(fm)),value=TRUE))
					pValue <- 1-pf(anova(fm)[tmp,'F value'],anova(fm)[tmp,'Df'],gl0)
					h2 <- varG/(varG + varErr/nRep)
					tmp <- round(c(h2,varG,varErr,media,LSD,CV,nRep,pValue),3)
					STATout[1:length(tmp),i+1] <- tmp
				}
				if(envir=="OVERALL"){
					gl0 <- (nGen-1)*(nLoc-1)
					LSD <- mean(SED$SE, na.rm=TRUE)*qt(1-0.05/2, round(gl0))
					tmp <- c("Entry",grep("Cov",rownames(anova(fm)),value=TRUE))
					pValue <- 1-pf(anova(fm)[tmp,'F value'],anova(fm)[tmp,'Df'],gl0)
					h2 <- varG/(varG + varGE/nLoc + varErr/(nRep*nLoc))
                              tmp <- round(c(h2,varLoc,varG,varGE,varErr,media,LSD,CV,nRep,nLoc,pValue),3)
					STATout[1:length(tmp),i+1] <- tmp
				}
  
				escribe_LOG(c("\t",rellena(traits[i]),"\t",round2(varG,4),"\t",round2(h2,4),"\t",round2(pValue[1],4)))
				}
				if(!checa$flag)
					escribe_LOG(c("\t",rellena(traits[i]),"\tNA\tNA\tNA\t",checa$mensaje,". Excluded from the analysis"))
				
				if(envir=="OVERALL" & length(toComb)<2 & checa$flag)
					escribe_LOG(c("\t",rellena(traits[i]),"\tNA\tNA\tNA\tNo enough locations to combine. Excluded from the analysis"))
	
 			}
			# If problems with covariates
			if(!flagCOV & !is.null(covariate)){
				escribe_LOG("",FileName="LOG1.txt")
				escribe_LOG("Warning:",FileName="LOG1.txt")
				for(k in 1:length(covariate)){
					if(!checaCOV[[k]]$flag) escribe_LOG(c("\t",checaCOV[[k]]$mensaje," in covariate '",covariate[k],"'"))
				}
				escribe_LOG(c("\t",ifelse(adjALL,"No variable was",paste(traits[1]," wasn't",sep=""))," adjusted by covariate"))
			}
			
			if(modelo=="fixed" & envir=="OVERALL") STATout <- STATout[STATout[,1]!="Location Variance",]
			results <- cbind(Statistic=rep("",nrow(results)),Location=rep(envir,nrow(results)),results) 
			tmp <- nrow(results)
			results <- rbind(results,matrix("",ncol=ncol(results),nrow=nrow(STATout)+2))
			results[(tmp+2):(tmp+nrow(STATout)+1),match(colnames(STATout),colnames(results))] <- STATout
			outMATRIX <- rbind(outMATRIX,results)
		}
		### Save results
		outFile <- paste(nombre,ifelse(typeAnalysis%in%c(3,5),paste("_",management,sep=""),""),
		ifelse(!is.null(covariate),"_withCovariate_","_withoutCovariate_"),ExpDes,".csv",sep="")
		write.csv(outMATRIX,outFile,row.names=FALSE,quote=FALSE)		
		if(!is.null(outH2)) write.csv(outH2,"Heritability_by_Location.csv",row.names=FALSE,quote=FALSE)
	}
	escribe_LOG("  ")
}