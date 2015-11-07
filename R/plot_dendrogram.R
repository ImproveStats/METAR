plot_dendrogram <-
function(hclust0,main=main,xlab="",ylab="")
{
  par(cex=0.77)
  plot(hclust0,main="",xlab="",hang=-0.15,sub="",yaxt="n",ylab="")
  par(cex=0.9)
  title(xlab=xlab, ylab=ylab, main=main)
}
