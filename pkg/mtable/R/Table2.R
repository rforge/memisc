Table2 <- function(x,by,...){
  if(any(is.na(by))) stop("NA(s) in 'by'")
  tab <- UseMethod("Table2")
  names(dimnames(tab))[1] <- deparse(substitute(x))
  tab
}



Table2.default <- function(x,
            by,weights=NULL,counts=TRUE,percentage=FALSE,...) {
#         message("Table2.default")
        if(!(counts || percentage)) stop("either counts or percentage must be TRUE")
          if(!length(weights)){
            tab <- drop(table(by,x))
          } else {
            good <- !is.missing(weights) & !is.missing(x) & !is.missing(by)
            unique.by <- unique(by[good])
            i <- match(by[good],unique.by)
            j <- x[good]
            ux <- sort(unique(j))
            j <- match(j,ux)
            tab <- rowsum2(weights[good],i,j,default=0)
          }

        if(percentage) {
          perc <- 100 * tab/rowSums(tab)
        }
        if(counts && percentage){
          ttab <- tab
          tab <- array(NA,c(dim(tab),2))
          tab[,,1] <- ttab
          tab[,,2] <- perc
          dimnames(tab) <- c(
                    dimnames(ttab),
                    list(c("Count","Percent"))
                    )
        }
        else if (percentage) {
          tab <- perc
        }
      if(length(tab)){
        dimnames(tab)[[1]] <- seq_len(dim(tab)[1])
        names(dimnames(tab))[1] <- "..by.."
        if(length(dim(tab))==2) tab <- t(tab)
        else tab <- aperm(tab,c(2,3,1))
        structure(tab,class="table")
        }
      else integer(0)
}

Table2.factor <- function(x,
            by,weights=NULL,counts=TRUE,percentage=FALSE,...) {
        if(!(counts || percentage)) stop("either counts or percentage must be TRUE")
          if(!length(weights)){
            tab <- drop(table(by,x))
          } else {
            good <- !is.missing(weights) & !is.missing(x) & !is.missing(by)
            by.good <- by[good]
            unique.by.good <- unique(by[good])
            i <- match(by.good,unique.by.good)
            j <- x[good]
            ux <- sort(unique(j))
            j <- match(j,ux)
            tmp <- rowsum2(weights[good],i,j,default=0)
            
            unique.by <- unique(by)
            tab <- array(0,dim=c(length(unique.by),ncol(tmp)))
            i <- match(unique.by.good,unique.by)
            tab[i,] <- tmp
            colnames(tab) <- levels(x)
          }

        if(percentage) {
          perc <- 100 * tab/rowSums(tab)
        }
        if(counts && percentage){
          ttab <- tab
          tab <- array(NA,c(dim(tab),2))
          tab[,,1] <- ttab
          tab[,,2] <- perc
          dimnames(tab) <- c(
                    dimnames(ttab),
                    list(c("Count","Percent"))
                    )
        }
        else if (percentage) {
          tab <- perc
        }
     if(length(tab)){
      dimnames(tab)[[1]] <- seq_len(dim(tab)[1])
      names(dimnames(tab))[1] <- "..by.."
      if(length(dim(tab))==2) tab <- t(tab)
      else tab <- aperm(tab,c(2,3,1))
      structure(tab,class="table")
      }
     else integer(0)
}


