marginal.table <- function(x,margin=NULL,FUN=sum,na.rm=FALSE,...){
    if (!is.array(x))
        stop("'x' is not an array")
    if (length(margin)) {
        if(inherits(margin,"formula")){
          if(length(margin)==3) {
            warning("dropping lhs of formula")
            margin <- margin[-2]
          }
          margin <- all.vars(margin)
          }
        if(is.character(margin))
          margin <- match(margin,names(dimnames(x)))
        z <- apply(x, margin, FUN,...)
        dim(z) <- dim(x)[margin]
        dimnames(z) <- dimnames(x)[margin]
    }
    else return(sum(x,na.rm=na.rm))
    class(z) <- oldClass(x)
    z
}


percent.table <- function(x,margin=NULL,na.rm=TRUE,...){
    if (!is.array(x))
        stop("'x' is not an array")
    if (length(margin)) {
        if(inherits(margin,"formula")){
          if(length(margin)==3) {
            x <- marginal.table(x,margin=rev(all.vars(margin)), na.rm=na.rm)
            margin <- margin[-2]
          }
          margin <- all.vars(margin)
        }
        if(is.character(margin))
          margin <- match(margin,names(dimnames(x)))
        m <- apply(x,margin,sum, na.rm=na.rm,...)
        x[] <- 100*sweep(x,margin,m,"/",check.margin=FALSE)
    }
    else
      x[] <- 100*x/sum(x)
    x
}