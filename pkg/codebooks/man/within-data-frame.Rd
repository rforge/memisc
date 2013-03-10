\name{within.data.frame}
\alias{within.data.frame}
\title{A Modified Version of 'within'}
\description{
    This is a modified version of the \code{data.frame} method of the generic function \code{\link[base]{within}}:
    If transformations in 'within' result in an item object, the function returns
        a 'data.set' object. 
    Variables in the
        resulting data frame or 'data.set' object will appear in the
        order of their creation by the transformation in the call
        of 'within'.
}
\usage{
    \method{within}{data.frame}(data, expr, \dots)
}
\arguments{
    \item{data}{a data frame.}
    \item{expr}{an expression to evaluate.}
    \item{\dots}{further arguments, currently ignored as in the default method.}
}