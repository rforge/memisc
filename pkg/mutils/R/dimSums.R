dimSums <- function(x,...) UseMethod("dimSums")

dimSums.array <- function(x,dims=1){

  sdx <- seq_along(dim(x))
  ksdx <- sdx[-dims]
  dsdx <- sdx[dims]

  rowSums(aperm(x,c(ksdx,dsdx)),
          dims=length(ksdx))
}
 
