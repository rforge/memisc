Table <- function(x,...) UseMethod("Table")

Table.default <- function(x,weights=NULL,counts=TRUE,percentage=FALSE,...) {
        if(!(counts || percentage)) stop("either counts or percentage must be TRUE")
          if(!length(weights)){
            tab <- drop(table(x))
          } else {
            good <- is.finite(weights) & is.finite(x)
            tmp <- rowsum(weights[good],x[good])
            tab <- drop(tmp)
          }

        if(percentage) {
          perc <- 100 * tab/sum(tab)
        }
        structure(if(counts && percentage)
            cbind(Counts=tab,Percent=perc)
          else if(percentage)
            perc
          else
            tab
            ,class="table")
}

Table.factor <- function(x,weights=NULL,counts=TRUE,percentage=FALSE,...) {
        if(!(counts || percentage)) stop("either counts or percentage must be TRUE")
          if(!length(weights)){
            tab <- drop(table(x))
          } else {
            good <- is.finite(weights) & is.finite(x)
            tmp <- rowsum(weights[good],x[good])
            tab <- structure(rep(0,nlevels(x[good])),names=levels(x[good]))
            tab[rownames(tmp)] <- tmp[]
          }
        if(percentage) {
          perc <- 100 * tab/sum(tab)
        }
        structure(if(counts && percentage)
            cbind(Counts=tab,Percent=perc)
          else if(percentage)
            perc
          else
            tab
            ,class="table")
}

