\name{relabel}
\alias{relabel}
\alias{relabel.default}
\alias{relabel.factor}
\title{Change labels of factors or labelled objects}
\description{
  Function \code{relabel} changes the labels of a factor or any object
  that has a \code{names}, \code{labels}, \code{value.labels}, or \code{variable.labels} attribute.
}
\usage{
\method{relabel}{default}(x, \dots, gsub = FALSE, fixed = TRUE, warn = TRUE)
\method{relabel}{factor}(x, \dots, gsub = FALSE, fixed = TRUE, warn = TRUE)
}
\arguments{
  \item{x}{An object with a \code{names}, \code{labels}, \code{value.labels}, or \code{variable.labels} attribute}
  \item{\dots}{A sequence of named arguments, all of type character}
  \item{gsub}{a logical value; if TRUE, \code{\link{gsub}} is used to change the
      labels of the object. That is, instead of substituting whole labels, substrings of the
      labels of the object can changed.}
  \item{fixed}{a logical value, passed to \code{\link{gsub}}. If TRUE,
    substitutions are by fixed strings and not by regular expressions.}
  \item{warn}{a logical value; if TRUE, a warning is issues if a
      a change of labels was unsuccessful.}
  }
\details{
  This function changes the names or labels of \code{x} according to the
  remaining arguments.
  If \code{gsub} is FALSE, argument tags are the \emph{old}
  labels, the values are the new labels.
  If \code{gsub} is TRUE, arguments are substrings of the labels
  that are substituted by the argument values.
}
\value{
  The object \code{x} with new labels defined by the \dots arguments.
}
\examples{
  f <- as.factor(rep(letters[1:4],5))
  levels(f)
  F <- relabel(f,
    a="A",
    b="B",
    c="C",
    d="D"
    )
  levels(F)
}


\keyword{manip}
