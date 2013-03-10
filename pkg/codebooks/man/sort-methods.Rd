\name{sort-methods}
\alias{sort.data.set}
\title{Convenience Methods to Sort Data Frames and Data Sets}
\description{
The method below returns a sorted version of
the data set given as first argument.
}
\usage{
\method{sort}{data.set}(x,decreasing=FALSE,by=NULL,na.last=NA,\dots)
}
\arguments{
  \item{x}{a data frame or data set.}
  \item{decreasing}{a logical value, should sorting
    be in increasing or decreasing order?}
  \item{by}{
    a character name of variable names, by which to sort;
    a formula giving the variables, by which to sort;
    NULL, in which case, the data frame / data set
    is sorted by all of its variables.
    }
  \item{na.last}{for controlling the treatment of 'NA's. If 'TRUE', missing
          values in the data are put last; if 'FALSE', they are put
          first; if 'NA', they are removed}
  \item{\dots}{other arguments, currently ignored.}
}
\value{
A sorted copy of \code{x}.
}
\examples{
DS <- data.set(
        a = sample(1:2,size=20,replace=TRUE),
        b = sample(1:4,size=20,replace=TRUE))
sort(DS)
sort(DS,by=~a+b)
sort(DS,by=~b+a)
sort(DS,by=c("b","a"))
sort(DS,by=c("a","b"))
}