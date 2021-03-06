% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distance(KthNearestNeighbour).R
\name{nnk}
\alias{nnk}
\title{Outlier detection using kth Nearest Neighbour Distance method}
\usage{
nnk(x, k = 0.05 * nrow(x), cutoff = 0.95, Method = "euclidean",
  rnames = FALSE, boottimes = 100)
}
\arguments{
\item{x}{dataset for which outliers are to be found}

\item{k}{No. of nearest neighbours to be used, default value is 0.05*nrow(x)}

\item{cutoff}{Percentile threshold used for distance, default value is 0.95}

\item{Method}{Distance method, default is Euclidean}

\item{rnames}{Logical value indicating whether the dataset has rownames, default value is False}

\item{boottimes}{Number of bootsrap samples to find the cutoff, default is 100 samples}
}
\value{
Outlier Observations: A matrix of outlier observations

Location of Outlier: Vector of Sr. no. of outliers

Outlier probability: Vector of proportion of times an outlier exceeds local bootstrap cutoff
}
\description{
Takes a dataset and finds its outliers using distance-based method
}
\details{
nnk computes kth nearest neighbour distance of an observation and based on the bootstrapped cutoff, labels an observation as outlier. Outlierliness of the labelled 'Outlier' is also reported and it is the bootstrap estimate of probability of the observation being an outlier. For bivariate data, it also shows the scatterplot of the data with labelled outliers.
}
\examples{
#Create dataset
X=iris[,1:4]
#Outlier detection
nnk(X,k=4)
}
\references{
Hautamaki, V., Karkkainen, I., and Franti, P. 2004. Outlier detection using k-nearest neighbour graph. In Proc. IEEE Int. Conf. on Pattern Recognition (ICPR), Cambridge, UK.
}
\author{
Vinay Tiwari, Akanksha Kashikar
}
