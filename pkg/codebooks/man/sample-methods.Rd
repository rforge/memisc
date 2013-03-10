\name{sample-methods}
\alias{sample,data.set-method}
\alias{sample,importer-method}
\title{Take a Sample from a Data Set or Importer Object}
\description{
  These methods are a convenience short-cuts to
  take samples from \code{\link{data.set}} or \code{\link{importer}} objects.
  They result in a data set (\code{\link{data.set}} object)
  the rows of which are a sample of the complete
  object.
}
\usage{
\S4method{sample}{data.set}(x, size, replace = FALSE, prob = NULL)
\S4method{sample}{importer}(x, size, replace = FALSE, prob = NULL)
}
\arguments{
  \item{x}{a data frame.}
  \item{size}{an (optional) numerical value, the sample size,
    defaults to the total number of rows of \code{x}.}
  \item{replace}{a logical value, determines whether
    sampling takes place with or without replacement.}
  \item{prob}{a vector of sampling probabities or NULL.}
}
\value{A data frame.}

\examples{
for(.i in 1:4)
  print(sample(iris,5))
}