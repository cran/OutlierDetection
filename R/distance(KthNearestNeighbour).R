#' Outlier detection using kth Nearest Neighbour Distance method
#'
#' Takes a dataset and find its outliers using distance-based method
#' @param x dataset for which outliers are to be found
#' @param k No. of nearest neighbours to be used, default value is 0.05*nrow(x)
#' @param cutoff Percentile threshold used for distance, default value is 0.95
#' @param Method Distance method, default is Euclidean
#' @param rnames Logical value indicating whether the dataset has rownames, default value is False
#' @param boottimes Number of bootsrap samples to find the cutoff, default is 100 samples

#' @details nnk computes kth nearest neighbour distance of an observation and based on the bootstrapped cutoff, labels an observation as outlier. Outlierliness of the labelled 'Outlier' is also reported and it is the bootstrap estimate of probability of the observation being an outlier. For bivariate data, it also shows the scatterplot of the data with labelled outliers.
#' @return Outlier Observations: A matrix of outlier observations
#' @return Location of Outlier: Vector of Sr. no. of outliers
#' @return Outlier probability: Vector of proportion of times an outlier exceeds local bootstrap cutoff
#' @references Hautamaki, V., Karkkainen, I., and Franti, P. 2004. Outlier detection using k-nearest neighbour graph. In Proc. IEEE Int. Conf. on Pattern Recognition (ICPR), Cambridge, UK.
#' @examples
#' #Create dataset
#' X=iris[,1:4]
#' #Outlier detection
#' nnk(X,k=4)

nnk=function(x,k=0.05*nrow(x),cutoff=.95,Method="euclidean",rnames=FALSE,boottimes=100)
{

  data=x
  dis=as.matrix(dist(data,diag=TRUE,upper = TRUE,method=Method))
  d=c()
  for (i in 1:nrow(data)) {
    temp=dis[,i]
    d[i]=sort(temp)[k]
  }
  bootub=c()
  for (j in 1:boottimes) {
    s=sample(1:length(d),length(d),replace = T)
    bootdata=d[s]
    bootub[j]=quantile(bootdata,cutoff)

  }
  ub=mean(bootub)
  wh=which(d>ub)
  out=data[wh,]
  loc=wh
  p=c()                             #outlier probability
  for (i in wh) {
    p[i]=length(which(bootub<d[i]))/length(bootub)
  }

  if(ncol(x)==2)
  {
    Class=as.factor(ifelse(d>ub,"Outlier","Normal"))
    cols <- c("Outlier" = "red", "Normal" = "blue")

    if(rnames==TRUE)
    {
      s=subset(data,Class=="Outlier")
      gplot=ggplot2::ggplot(data,aes(data[,1],data[,2]))+geom_point(aes(colour=Class,pch=Class))+geom_text(data=s,aes(x=s[,1],y=s[,2],label=rownames(s)),colour="Red", hjust = "inward",check_overlap = T)+ggtitle("Outlier plot using kth nearest neighbour distance")+xlab("Variable1")+ylab("Variable2")+scale_color_manual(values=cols)

    }else
    {dd=cbind(data,1:nrow(data))
    s=subset(dd,Class=="Outlier")
    gplot=ggplot2::ggplot(data,aes(data[,1],data[,2]))+geom_point(aes(colour=Class,pch=Class))+geom_text(data=s,aes(x=s[,1],y=s[,2],label=s[,3]),colour="Red", hjust = "inward",check_overlap = T)+ggtitle("Outlier plot using kth nearest neighbour distance")+xlab("Variable1")+ylab("Variable2")+scale_color_manual(values=cols)
    gplot
    }
    l=list("Outlier Observations"=out,"Location of Outlier"=loc,"Outlier Probability"=p[is.na(p)==F],"Scatter plot"=gplot)
  }else

  l=list("Outlier Observations"=out,"Location of Outlier"=loc,"Outlier Probability"=p[is.na(p)==F])
  return(l)
}
