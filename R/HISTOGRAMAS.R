HISTOGRAMAS <-
function()
{
	folderPlot <- "Histograms"
	if(!file.exists(folderPlot))  dir.create(folderPlot)
	cex0 <- 0.77
	for(i in 1:length(traits))
	{
		if("Manag"%in%var.class)
		{
			datos1 <- split(datos,as.character(datos[,"Manag"]))
			for(j in 1:length(datos1))
			{
				tmp <- datos1[[j]]
				tmp <- suppressWarnings(data.frame(Loc=tmp[,"Loc"],Rep=as.character(tmp[,"Rep"]),y=as.numeric(as.character(tmp[,traits[i]]))))
				to.plot <- split(tmp,as.character(tmp[,"Loc"]))
				flag <- unlist(lapply(to.plot,function(x)any(!is.na(x$y))))
				if(sum(flag)>0)
				{
					for(k in 1:length(to.plot))
					{
						main <- paste("Histogram by replication\n",names(datos1)[j],". ",names(to.plot)[k],sep="")
						if(flag[k])
						{	
							prefix <- paste(folderPlot,"/Histogram_",names(datos1)[j],"_",names(to.plot)[k],"_",traits[i],sep="")
							if(histogramasFormat=="pdf") pdf(paste(prefix,".pdf",sep=""))
							if(histogramasFormat=="wmf") win.metafile(paste(prefix,".wmf",sep=""))
							if(histogramasFormat=="png") png(paste(prefix,".png",sep=""))
							histo2(to.plot[[k]],main=main)
							dev.off()
						}
					}
				}
			}
		}

		tmp <- suppressWarnings(data.frame(Loc=datos[,"Loc"],Rep=as.character(datos[,"Rep"]),y=as.numeric(as.character(datos[,traits[i]]))))
		to.plot <- split(tmp,as.character(tmp[,"Loc"]))
		flag <- unlist(lapply(to.plot,function(x)any(!is.na(x$y))))
		if(sum(flag)>0)
		{
			for(k in 1:length(to.plot))
			{
				if(flag[k]){
					if(histogramasFormat=="pdf") pdf(paste(folderPlot,"/Histogram_",names(to.plot)[k],"_",traits[i],".pdf",sep=""))
					if(histogramasFormat=="wmf") win.metafile(paste(folderPlot,"/Histogram_",names(to.plot)[k],"_",traits[i],".wmf",sep=""))
					if(histogramasFormat=="png") png(paste(folderPlot,"/Histogram_",names(to.plot)[k],"_",traits[i],".png",sep=""))
					main <- paste("Histogram by replication\n",names(to.plot)[k],sep="")
					histo2(to.plot[[k]],main=main)
					dev.off()
				}
			}
		}
	}
	escribe_LOG("Histograms done")
}
