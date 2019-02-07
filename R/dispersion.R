#' Outlier detection using genralised dispersion
#'
#' Takes a dataset and find its outliers using dispersion-based method
#' @param x dataset for which outliers are to be found
#' @param cutoff Percentile threshold used for distance, default value is 0.95
#' @param rnames Logical value indicating whether the dataset has rownames, default value is False
#' @param boottimes Number of bootsrap samples to find the cutoff, default is 100 samples
#' @details disp computes LOO dispersion matrix for each observation(dispersion matrix without cosidering the current observation) and based on the bootstrapped cutoff for score(difference between determinant of LOO dispersion matrix and det of actual dispersion matrix), labels an observation as outlier. Outlierliness of the labelled 'Outlier' is also reported and it is the bootstrap estimate of probability of the observation being an outlier. For bivariate data, it also shows the scatterplot of the data with labelled outliers.
#' @return Outlier Observations: A matrix of outlier observations
#' @return Location of Outlier: Vector of Sr. no. of outliers
#' @return Outlier probability: Vector of proportion of times an outlier exceeds local bootstrap cutoff
#' @references Jin, W., Tung, A., and Han, J. 2001. Mining top-n local outliers in large databases. In Proc. ACM SIGKDD Int. Conf. on Knowledge Discovery and Data Mining (SIGKDD), San Francisco, CA.
#' @examples
#' #Create dataset
#' X=iris[,1:4]
#' #Outlier detection
#' disp(X,cutoff=0.99)

disp=function(x,cutoff=.95,rnames=FALSE,boottimes=100)
{

  data=x;d=c()
  for (i in 1:nrow(data)) {
    remdata=data[-i,]
    remCov=cov(remdata)
    d[i]=abs(det(remCov)-det(cov(x)))
  }
  bootub=c()
  for (j in 1:boottimes) {
    s=sample(1:length(d),length(d),replace = T)
    bootdata=d[s]
    bootub[j]=quantile(bootdata,0.95)

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
      gplot=ggplot2::ggplot(data,aes(data[,1],data[,2]))+geom_point(aes(colour=Class,pch=Class))+geom_text(data=s,aes(x=s[,1],y=s[,2],label=rownames(s)),colour="Red", hjust = "inward",check_overlap = T)+ggtitle("Outlier plot using dispersion")+xlab("Variable1")+ylab("Variable2")+scale_color_manual(values=cols)
      }else
    {dd=cbind(data,1:nrow(data))
    s=subset(dd,Class=="Outlier")
    gplot=ggplot2::ggplot(data,aes(data[,1],data[,2]))+geom_point(aes(colour=Class,pch=Class))+geom_text(data=s,aes(x=s[,1],y=s[,2],label=s[,3]),colour="Red", hjust = "inward",check_overlap = T)+ggtitle("Outlier plot using dispersion")+xlab("Variable1")+ylab("Variable2")+scale_color_manual(values=cols)
    }
    l=list("Outlier Observations"=out,"Location of Outlier"=loc,"Outlier Probability"=p[is.na(p)==F],"Scatter plot"=gplot)
  }else
  l=list("Outlier Observations"=out,"Location of Outlier"=loc,"Outlier Probability"=p[is.na(p)==F])
  return(l)
}

