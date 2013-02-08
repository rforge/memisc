\name{foreach}
\alias{foreach}
\title{Loop over Variables in a Data Frame or Environment}
\description{
  \code{foreach} evaluates an expression given as second argument by substituting
  in variables. The expression may also contain assignments, which take effect in
  the callers environment.
}
\usage{
  foreach(\dots)
}
\arguments{
  \item{\dots}{tagged and untagged arguments.
    The tagged arguments define the 'variables' that are looped over,
    the first untagged argument defines the expression wich is
    evaluated.
  }
}
\examples{
x <- 1:3
y <- -(1:3)
z <- c("Uri","Schwyz","Unterwalden")
print(x)
print(y)
print(z)
foreach(var=c(x,y,z),          # assigns names
  names(var) <- letters[1:3]   # to the elements of x, y, and z
  )
print(x)
print(y)
print(z)
}
\keyword{programming}