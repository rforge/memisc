\name{Table}
\alias{Table}
\alias{Table.default}
\alias{Table.factor}
\alias{Table2}
\title{One-Dimensional Table of Frequences and/or Percentages}
\description{
  \code{Table} is a generic function that
  produces a table of counts or weighted counts
  and/or the corresponding percentages of an atomic vector,
  factor or \code{"item.vector"} object.
  This function is intended for use with
  \code{\link{Aggregate}} or \code{\link{genTable}}.
  }
\usage{
\S3method{Table}{default}(x,weights=NULL,counts=TRUE,percentage=FALSE,\dots)
\S3method{Table}{factor}(x,weights=NULL,counts=TRUE,percentage=FALSE,\dots)
}
\arguments{
  \item{x}{an atomic vector, factor or \code{"item.vector"} object}
  \item{counts}{logical value, should the table contain counts?}
  \item{percentage}{logical value, should the table contain percentages?
  Either the \code{counts} or the \code{percentage} arguments or both
  should be \code{TRUE}. }
  \item{weights}{a numeric vector of weights of the same length as \code{x}.}
  \item{\dots}{other, currently ignored arguments.}
}
\value{
  The atomic vector and factor methods return either a vector
  of counts or vector of percentages or a matrix of counts and percentages.
}
\examples{
  with(as.data.frame(UCBAdmissions),Table(Admit,Freq))
  Aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)

  A <- sample(c(1:5),size=100,replace=TRUE)
  A <- factor(A,labels=letters[1:5])
  Table(A,percentage=TRUE)
}
\keyword{univar}
