#' @title scatterplots
#'
#' @description Generates the pairwise correlation between the samples
#'
#' @param expr.matrix the raw or normlaised count matrix
#'
#' @return plots
#'
#' @examples
#'
#' @export scatterplots



scatterplots=function(file, directory=getwd())
{
IDs <-colnames(file)
for (i in 1:(dim(file)[2]-1))
{
for( j in i:(dim(file)[2]) )
{
if (i != j)
{
jpeg(file=paste(directory,"/",IDs[i],"_gegen_",IDs[j],".jpg",sep=""))
correlation<-round(cor(file[,i],file[,j]),2)
maximum<- max(log2(file[,i]))
minimum<-min(log2(file[,i]))
plot(log2 (file[,i]), log2 (file[,j]), xlab= IDs[i], ylab= IDs[j], pch='.', text(maximum - 2, minimum + 0.5, labels = paste ("R = ", correlation, sep= ""), pos= 4, offset = 0))
dev.off()
}
}
}
}
