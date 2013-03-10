cases <- function(...,check.xor=FALSE){
  subst <- match.call(expand.dots=FALSE)$...
  deflabels <- sapply(subst,deparse)
  if(length(subst)<2) stop("need at least two conditions")

  have.arrows <- sapply(subst,length) > 1
  have.arrows[have.arrows] <- have.arrows[have.arrows] & sapply(sapply(subst[have.arrows],"[[",1),paste)=="<-"

  parent <- parent.frame()
  if(all(have.arrows)){

    conditions <- lapply(subst,"[[",3)
    values <- lapply(subst,"[[",2)
    conditions <- do.call(cbind,lapply(conditions,eval,envir=parent))
    if(!is.logical(conditions)) stop("all conditions have to be logical")
    #if(any(is.na(conditions))) stop("NA in logical condition")
    na.cond <- rowSums(is.na(conditions)) > 0

    done <- rowSums(conditions,na.rm=TRUE)
    if(check.xor && (any(done!=1) || any(na.cond>0))) stop("conditions are neither exclusive nor exhaustive")
    conditions[is.na(conditions)] <- FALSE
    never <- colSums(conditions) == 0
    if(any(never)){
      neverlab <- deflabels[never]
      if(length(neverlab)==1)
        warning("condition ",neverlab," is never satisfied")
      else
        warning("conditions ",paste(neverlab,collapse=", ")," are never satisfied")
    }

    values <- lapply(values,eval,envir=parent.frame(),enclos=parent.frame())
    nrow <- unique(sapply(values,length))
    if(length(nrow) > 1 || nrow != nrow(conditions)){
      nrow <- nrow(conditions)
      values <- lapply(values,function(x){
        tmp <- x
        length(tmp) <- nrow
        tmp[] <- x
        tmp
        })
    }
    values <- do.call(cbind,values)
    res <- vector(nrow(conditions),mode=storage.mode(values))
    
    for(i in rev(1:ncol(conditions))){
      res[conditions[,i]] <- values[conditions[,i],i]
    }
    res[!done] <- as.vector(NA,mode=storage.mode(values))
    res
  }
  else if(!any(have.arrows))
  {
    conditions <- cbind(...)
    if(!is.logical(conditions)) stop("all conditions have to be logical")
    #if(any(is.na(conditions))) stop("NA in logical condition")
    na.cond <- rowSums(is.na(conditions)) > 0

    codes <- 1:ncol(conditions)
    labels <- colnames(conditions)
    if(length(labels))
      labels <- ifelse(nzchar(labels),labels,deflabels)
    else labels <- deflabels

    done <- rowSums(conditions,na.rm=TRUE)
    if(check.xor && (any(done!=1) || any(na.cond>0))) stop("conditions are neither exclusive nor exhaustive")
    conditions[is.na(conditions)] <- FALSE
    never <- colSums(conditions) == 0
    if(any(never)){
      neverlab <- deflabels[never]
      if(length(neverlab)==1)
        warning("condition ",neverlab," is never satisfied")
      else
        warning("conditions ",paste(neverlab,collapse=", ")," are never satisfied")
    }
    res <- integer(nrow(conditions))
    
    for(i in rev(1:ncol(conditions))){
      res[conditions[,i]] <- i
    }
    res[!done] <- NA_integer_
    factor(res,levels=codes,labels=labels)
  }
  else stop("inconsistent arguments to 'cases'")
}
