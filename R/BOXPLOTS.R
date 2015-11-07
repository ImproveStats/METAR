BOXPLOTS <-
function()
{
	folderPlot <- "Boxplots"
	if(!file.exists(folderPlot))  dir.create(folderPlot)
	cex0 <- 0.77
	dec <- 2

	if("Manag"%in%var.class)
	{
		datos1 <- split(datos,as.character(datos[,"Manag"]))
		for(j in 1:length(datos1))
		{
				tmp <- datos1[[j]]
				nEnv <- length(unique(as.character(tmp[,"Loc"])))
				if(nEnv>10)	escribe_LOG(paste(" - Management: ",names(datos1)[j],". There are more than 10 environments. Only the first 10 will be plotted...",sep=""))
		}

	}
	nEnv <- length(unique(as.character(datos[,"Loc"])))
	if(nEnv>10)	escribe_LOG(" - All environments. There are more than 10 environments. Only the first 10 will be plotted...")
	
	for(i in 1:length(traits))
	{
		if("Manag"%in%var.class)
		{
			datos1 <- split(datos,as.character(datos[,"Manag"]))
			for(j in 1:length(datos1))
			{
				tmp <- datos1[[j]]
				nEnv <- length(unique(as.character(tmp[,"Loc"])))
				tmp <- suppressWarnings(data.frame(Loc=tmp[,"Loc"],y=as.numeric(as.character(tmp[,traits[i]]))))
				to.plot <- split(tmp[,'y'],as.character(tmp[,"Loc"]))
				if(nEnv>10)	to.plot <- to.plot[1:10]
				flag <- unlist(lapply(to.plot,function(x)any(!is.na(x))))
				if(sum(flag)>0)
				{
				width <- ifelse(length(to.plot)<5,480,95*length(to.plot))
				prefix <- paste(folderPlot,"/Boxplot_",names(datos1)[j],"_",traits[i],sep="")
				if(box_plotFormat=="pdf") pdf(paste(prefix,".pdf",sep=""),height=7.6,width=7*width/480)
				if(box_plotFormat=="wmf") win.metafile(paste(prefix,".wmf",sep=""),height=7.6,width=7*width/480)
				if(box_plotFormat=="png") png(paste(prefix,".png",sep=""),height=520,width=width)
				main <- paste("Boxplot. ",names(datos1)[j],sep="")
				boxp2(to.plot,dec=dec,main=main,ylab=traits[i],cex0=cex0)
				dev.off()
				}
			}
		}

		nEnv <- length(unique(as.character(datos[,"Loc"])))
		tmp <- suppressWarnings(data.frame(Loc=datos[,"Loc"],y=as.numeric(as.character(datos[,traits[i]]))))
		to.plot <- split(tmp[,'y'],as.character(tmp[,"Loc"]))
		if(nEnv>10)	to.plot <- to.plot[1:10]
		flag <- unlist(lapply(to.plot,function(x)any(!is.na(x))))
		if(sum(flag)>0)
		{
			width <- ifelse(length(to.plot)<5,480,95*length(to.plot))
			if(box_plotFormat=="pdf") pdf(paste(folderPlot,"/Boxplot_AllLocations_",traits[i],".pdf",sep=""),height=7.6,width=7*width/480)
			if(box_plotFormat=="wmf") win.metafile(paste(folderPlot,"/Boxplot_AllLocations_",traits[i],".wmf",sep=""),height=7.6,width=7*width/480)
			if(box_plotFormat=="png") png(paste(folderPlot,"/Boxplot_AllLocations_",traits[i],".png",sep=""),height=520,width=width)
			main <- "Boxplot. All Locations"
			boxp2(to.plot,dec=dec,main=main,ylab=traits[i],cex0=cex0)
			dev.off()
		}		
	}
	escribe_LOG("Boxplots done")
}
