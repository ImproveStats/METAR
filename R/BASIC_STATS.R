BASIC_STATS <-
function()
{
	out <- c()
	for(i in 1:length(traits))
	{
		if("Manag"%in%var.class)
		{
			datos1 <- split(datos,as.character(datos[,"Manag"]))
			for(j in 1:length(datos1))
			{
				tmp <- datos1[[j]]
				tmp <- suppressWarnings(data.frame(Loc=tmp[,"Loc"],y=as.numeric(as.character(tmp[,traits[i]]))))
				to.plot <- split(tmp[,'y'],as.character(tmp[,"Loc"]))
				medias <- round(unlist(lapply(to.plot,mean,na.rm=TRUE)),4)
				conteo <- round(unlist(lapply(to.plot,length)),4)
				nNA <- round(unlist(lapply(to.plot,function(x)sum(is.na(x)))),4)
				sdesv <- round(unlist(lapply(to.plot,sd,na.rm=TRUE)),4)
				mins <- suppressWarnings(round(unlist(lapply(to.plot,min,na.rm=TRUE)),4))
				maxs <- suppressWarnings(round(unlist(lapply(to.plot,max,na.rm=TRUE)),4))
				quantile25 <- round(unlist(lapply(to.plot,quantile,prob=0.25,na.rm=TRUE)),4)
				medians <- round(unlist(lapply(to.plot,median,na.rm=TRUE)),4)
				quantile75 <- round(unlist(lapply(to.plot,quantile,prob=0.75,na.rm=TRUE)),4)
				tmp <- cbind(conteo,nNA,mins,quantile25,medians,quantile75,maxs,medias,sdesv)
				tmp[is.infinite(tmp) | is.na(tmp)] <- "-"
				tmp <- cbind(rep(traits[i],length(to.plot)),rep(names(datos1)[j],length(to.plot)),names(mins),tmp)
				out <- rbind(out,tmp)
			}
		}
		tmp <- suppressWarnings(data.frame(Loc=datos[,"Loc"],y=as.numeric(as.character(datos[,traits[i]]))))
		to.plot <- split(tmp[,'y'],as.character(tmp[,"Loc"]))
		medias <- round(unlist(lapply(to.plot,mean,na.rm=TRUE)),4)
		conteo <- round(unlist(lapply(to.plot,length)),4)
		nNA <- round(unlist(lapply(to.plot,function(x)sum(is.na(x)))),4)
		sdesv <- round(unlist(lapply(to.plot,sd,na.rm=TRUE)),4)
		mins <- suppressWarnings(round(unlist(lapply(to.plot,min,na.rm=TRUE)),4))
		maxs <- suppressWarnings(round(unlist(lapply(to.plot,max,na.rm=TRUE)),4))
		quantile25 <- round(unlist(lapply(to.plot,quantile,prob=0.25,na.rm=TRUE)),4)
		medians <- round(unlist(lapply(to.plot,median,na.rm=TRUE)),4)
		quantile75 <- round(unlist(lapply(to.plot,quantile,prob=0.75,na.rm=TRUE)),4)
		tmp <- cbind(conteo,nNA,mins,quantile25,medians,quantile75,maxs,medias,sdesv)
		tmp[is.infinite(tmp) | is.na(tmp)] <- "-"
		tmp <- cbind(rep(traits[i],length(to.plot)),rep("-",length(to.plot)),names(mins),tmp)
		out <- rbind(out,tmp,rep("",ncol(tmp)))
	}
	colnames(out) <- c("Trait","Management","Location","N","N_Missing","Minimum","Quantile25","Median","Quantile75",
	"Maximum","Mean","Std Dev")
	write.table(out,"Basic_Statistics.csv",sep=",",quote=FALSE,row.names=FALSE)
}
