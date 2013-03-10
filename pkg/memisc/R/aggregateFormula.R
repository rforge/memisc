aggregate.formula <- function (x,
                        data = parent.frame(),
                        subset = NULL,
                        sort=TRUE,
                        names=NULL,
                        addFreq=TRUE,
                        as.vars=1,
                        drop.constants=TRUE,
                        ...){
      m <- match.call()
      m[[1]] <- as.name("Aggregate")
      names(m)[2] <- "formula"
      m$labels <- m$names
      m$names <- NULL
      eval(m, parent.frame())
}