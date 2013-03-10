\name{sample-methods}
\alias{sample,data.frame-method}
\title{Take a Sample from a Data Frame}
\description{
  This method is a convenience short-cut to
  take samples from data frames.
  It results in a data frame
  the rows of which are a sample of the complete
  data frame.
}
\usage{
\S4method{sample}{data.frame}(x, size, replace = FALSE, prob = NULL)
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