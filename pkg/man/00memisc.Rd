\name{Memisc}
\docType{package}
\alias{memisc-package}
\alias{memisc}
\title{Introduction to the `memisc' Package}
\description{This package collects an assortment of tools that are intended to make
work with \code{R} easier for the author of this package
and are submitted to the public in the hope that they will be also be useful to others.

The tools in this package can be grouped into five major categories:
\itemize{
  \item Data preparation and management
  \item Data analysis
  \item Presentation of analysis results
  \item Simulation
  \item Programming
}
}
\section{Data preparation and management}{

  \pkg{memisc} contains facilities for recoding
  survey items. Simple recodings, for example collapsing answer
  categories, can be done using the function \code{\link{recode}}. More
  complex recodings, for example the construction of indices from
  multiple items, and complex case distinctions, can be done
  using the function \code{\link{cases}}. This function may also
  be useful for programming, in so far as it is a generalization of
  \code{\link{ifelse}}. An extensive infrastructure to
  handle `variable labels', `value labels', and `user-defined missing values'
  as one may know them from Stata or SPSS are provided in a companion package
  called \pkg{codebooks}.

  }

\section{Data Analysis}{
\subsection{Tables and Data Frames of Descriptive Statistics}{

  \code{\link{genTable}} is a generalization of \code{\link{xtabs}}:
  Instead of counts, also descriptive statistics like means or variances
  can be reported conditional on levels of factors. Also conditional
  percentages of a factor can be obtained using this function.
  \code{\link{Aggregate}} is a variation of \code{genTable} that returns
  a data frame instead of a table.
  }

\subsection{Per-Subset Analysis}{

  \code{\link{By}} is a variant of the
  standard function \code{\link[base]{by}}: Conditioning factors
  are specified by a formula and are
  obtained from the data frame the subsets of which are to be analysed.
  Therefore there is no need to \code{\link{attach}} the data frame
  or to use the dollar operator.
}
\subsection{Graphical Model Comparison}{

  \code{\link{Termplot}} is a variant and an extension of \code{\link[stats]{termplot}}:
  The plots are similar to those of \code{termplot} but uses \code{\link[lattice]{lattice}}
  graphics. Also \code{Termplot} can be used on more than one model and allows
  to compare the fit of linear or non-linear effect specifications of different
  models.

  Use \code{example(Termplot)} or \code{demo(Termplot)} for an example.
}
}
\section{Presentation of Results of Statistical Analysis}{
\subsection{Publication-Ready Tables of Coefficients}{

Journals of the Political and Social Sciences usually require
that estimates of regression models are presented in the following
form:
\preformatted{
    ==================================================
                    Model 1     Model 2     Model 3
    --------------------------------------------------
    Coefficients
    (Intercept)     30.628***    6.360***   28.566***
                    (7.409)     (1.252)     (7.355)
    pop15           -0.471**                -0.461**
                    (0.147)                 (0.145)
    pop75           -1.934                  -1.691
                    (1.041)                 (1.084)
    dpi                          0.001      -0.000
                                (0.001)     (0.001)
    ddpi                         0.529*      0.410*
                                (0.210)     (0.196)
    --------------------------------------------------
    Summaries
    R-squared         0.262       0.162       0.338
    adj. R-squared    0.230       0.126       0.280
    N                50          50          50
    ==================================================
}

Such tables of coefficient estimates can be produced
by \code{\link{mtable}}. To see some of the possibilities of
this function, use \code{example(mtable)}.
}

\subsection{LaTeX Representation of R Objects}{

Output produced by \code{\link{mtable}} can be transformed into
LaTeX tables by an appropriate method of the generic function
\code{\link[utils]{toLatex}} which is defined in the package
\code{utils}. In addition, \code{memisc} defines \code{toLatex} methods
for matrices and \code{\link[stats]{ftable}} objects. Note that
results produced by \code{\link{genTable}} can be coerced into
\code{\link[stats]{ftable}} objects. Also, a default method
for the \code{toLatex} function is defined which coerces its
argument to a matrix and applies the matrix method of \code{toLatex}.
}
}
\section{Simulation}{
The \code{memisc} package defines a function \code{\link{Simulate}},
which can be used to conduct simulation experiments: For a given
number of replications and given sets of parameters (which can
be varied across experimental conditions) data are generated and
can summarized afterwards by other methods.

Use \code{example(Simulate)}, \code{demo(monte.carlo)}, \code{demo(lm.monte.carlo)},
\code{demo(random.walk)}, or \code{demo(schellings)} for examples.
}
\section{Programming}{
\subsection{Looping over Variables}{

  Sometimes users want to contruct loops that run over variables rather than values.
  For example, if one wants to set the missing values of a battery of items.
  For this purpose, the package contains the function \code{\link{Foreach}}.
  To set 8 and 9 as missing values for the items \code{knowledge1},
  \code{knowledge2}, \code{knowledge3}, one can use
  \preformatted{
    Foreach(x=c(knowledge1,knowledge2,knowledge3),
        x[x \%in\% 8:9] <- NA)
  }
}
\subsection{Changing Names of Objects and Labels of Factors}{

  \code{R} already makes it possible to change the names of an object.
  Substituting the \code{\link[base]{names}} or \code{\link[base]{dimnames}}
  can be done with some programming tricks. This package defines
  the function \code{\link{rename}},
  \code{\link{dimrename}}, \code{\link{colrename}}, and \code{\link{rowrename}}
  that implement these tricks in a convenient way, so that programmers
  (like the author of this package) need not reinvent the weel in
  every instance of changing names of an object.
}
\subsection{Combining Vectors and Arrays by Names}{

The generic function \code{\link{collect}} collects several objects of the
same mode into one object, using their names, \code{rownames},
\code{colnames} and/or \code{dimnames}. There are methods for
atomic vectors, arrays (including matrices), and data frames.
For example
\preformatted{
x <- c(a=1,b=2)
y <- c(a=10,c=30)
collect(x,y)
}
leads to
\preformatted{
   x  y
a  1 10
b  2 NA
c NA 30
}
}
\subsection{Reordering of Matrices and Arrays}{

  The \code{memisc} package includes a \code{\link{reorder}}
  method for arrays and matrices. For example, the matrix
  method by default reorders the rows of a matrix according the results
  of a function.
  
}
}

\keyword{misc}