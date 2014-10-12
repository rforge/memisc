rowOuter <- function(x,y=x){

  x <- as.matrix(x)
  y <- as.matrix(y)
  ncol.x <- ncol(x)
  ncol.y <- ncol(y)
  nrows <- nrow(x)
  stopifnot(nrows==nrow(y))

  x <- array(x,dim=c(nrows,ncol.x,ncol.y))
  y <- aperm(array(y,dim=c(nrows,ncol.y,ncol.x)),
             c(1,3,2))
  x*y
} 

genOuter <- function(x,y=x,along.x=1,along.y=1){

  x <- as.array(x)
  y <- as.array(y)

  adx <- dim(x)[along.x]
  ady <- dim(y)[along.y]
  if(!all(adx==ady)) stop("dimensions do not match")

  adnx <- dimnames(x)[along.x]
  adny <- dimnames(y)[along.y]

  dx <- dim(x)[-along.x]
  dy <- dim(y)[-along.y]

  dnx <- dimnames(x)[-along.x]
  dny <- dimnames(y)[-along.y]

  x <- as.matrix.array(x,as.rows=along.x)
  y <- as.matrix.array(y,as.rows=along.y)

  z <- semi.outer.mprod(x,y)

  dxy <- c(adx,dx,dy)

  if(length(dnx) || length(dny)){

    if(is.null(dnx)) dnx <- if(length(dx)) rep(list(NULL),length(dx))
                            else NULL
    if(is.null(dny)) dny <- if(length(dy)) rep(list(NULL),length(dy))
                            else NULL
    dnxy <- c(dnx,dny)
  } else dnxy <- NULL

  if(length(adnx) && length(adny)){

    adnxy <- adnx
    adnxy.null <- sapply(adnxy,is.null)
    adnxy[adnxy.null] <- adny[adnxy.null]
  }
  else if(length(adnx)) adnxy <- adnx
  else if(length(adny)) adnxy <- adny
  else if(length(dnxy))
    adnxy <- rep(list(NULL),length(dxy))
  else adnxy <- NULL

  dnxy <- c(adnxy,dnxy)

  dim(z) <- dxy
  dimnames(z) <- dnxy
  z
}
