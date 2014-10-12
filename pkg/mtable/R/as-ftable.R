as.ftable <- function(x,...){
  UseMethod("as.ftable")
}

as.ftable.default <- function(x,labels=NULL,name=NULL,...){
  if(!is.array(x)) {
    x <- as.matrix(x)
    if(length(labels)==2& !length(name)){
      name <- labels[2]
      labels[2] <- ""
    }
    colnames(x) <- name
  }
  if(length(labels)) 
    names(dimnames(x)) <- labels
  x <- as.table(x)
  ftable(x,...)
}

as.ftable.table <- function(x,...){
  ftable(x,...)
}
