setMethod("recode","vector",function(x,...,otherwise=NA){
  call <- match.call()
  if("recodes" %in% names(call)
    || is.character(call[[3]])
    ) {
      return(car_recode(var=x,...))
    }
  recodings <- match.call(expand.dots=FALSE)$...
  if("to.factor" %in% names(recodings)
    && is.logical(recodings[["to.factor"]])
    ) {
      warning("'to.factor' argument no longer in use")
      recodings["to.factor"] <- NULL
    }
  recodings <- recodings[nzchar(sapply(recodings,paste,collapse=""))]
  if(any(sapply(sapply(recodings,"[[",1),paste)!="<-"))
    stop("invalid recoding request")

  if(!length(recodings)) return(x)
  newcodes <- lapply(recodings,"[[",2)
  oldcodes <- lapply(recodings,"[[",3)
  has.range <- paste(lapply(oldcodes,"[[",1)) == "range"
  if(any(has.range)){
    has.min <- paste(lapply(oldcodes[has.range],"[[",2)) == "min"
    has.max <- paste(lapply(oldcodes[has.range],"[[",3)) == "max"
    if(any(has.min)){
      min.list <- list(min=min(x, na.rm=TRUE))
      oldcodes[has.range][has.min] <- lapply(
                oldcodes[has.range][has.min],
                function(x) do.call("substitute",list(x,min.list))
                )
    }
    if(any(has.max)){
      max.list <- list(max=max(x, na.rm=TRUE))
      oldcodes[has.range][has.max] <- lapply(
                oldcodes[has.range][has.max],
                function(x) do.call("substitute",list(x,max.list))
                )
    }
  }
  oldcodes[has.range] <- lapply(oldcodes[has.range],
                          function(x)
                          as.call(list(as.symbol("&"),
                            as.call(list(as.symbol("<="),x[[2]],as.symbol("x"))),
                            as.call(list(as.symbol("<="),as.symbol("x"),x[[3]]))
                            )
                          ))
  oldcodes[!has.range] <- lapply(oldcodes[!has.range],
                          function(x)
                          as.call(list(as.symbol("%in%"),as.symbol("x"),x))
                          )
  torecode <- sapply(oldcodes,eval,envir=environment())
  if(!is.matrix(torecode)) torecode <- t(oldcodes)
  newcodes <- sapply(newcodes,eval,parent.frame())
  nevtrue <- colSums(torecode, na.rm=TRUE) == 0
  if(any(nevtrue)){
    nrecng <- paste(recodings[nevtrue])
    if(length(nrecng)>1)
      warning("recodings ",paste(nrecng,collapse=", ")," have no consequences")
    else
      warning("recoding ",nrecng," has no consequences")
  }
  ambiguous <- rowSums(torecode, na.rm=TRUE) > 1
  if(any(ambiguous))
    stop("recoding request is ambiguous")
  y <- if(is.character(newcodes)) as.character(x) else x
  newcodes <- newcodes[!nevtrue]
  torecode <- torecode[,!nevtrue,drop=FALSE]
  for(i in seq(along=newcodes)){
    y[torecode[,i]] <- newcodes[i]
  }
  if(is.na(otherwise) || otherwise!="copy"){
	if(is.character(otherwise)) newcodes <- c(newcodes, otherwise)
    recoded <- as.logical(rowSums(torecode))
	recoded[is.na(recoded)] <- TRUE
    tmp <- as.vector(otherwise,mode=storage.mode(y))
    length(otherwise) <- length(y)
    otherwise[] <- tmp
    y[!recoded] <- otherwise[!recoded]
  }
  if(is.character(y)) y <- factor(y,levels=newcodes)
  return(y)
})

setMethod("recode","factor",function(x,...,otherwise=NA){
  recodings <- match.call(expand.dots=FALSE)$...
  if("to.factor" %in% names(recodings)
    && is.logical(recodings[["to.factor"]])
    ) {
      warning("'to.factor' argument no longer in use")
      recodings["to.factor"] <- NULL
    }
  recodings <- recodings[nzchar(sapply(recodings,paste,collapse=""))]
  if(any(sapply(sapply(recodings,"[[",1),paste)!="<-"))
    stop("invalid recoding request")

  if(!length(recodings)) return(x)
  newcodes <- sapply(recodings,"[[",2)
  newcodes <- as.character(newcodes)
  oldcodes <- lapply(recodings,"[[",3)
  oldcodes <- lapply(oldcodes,eval,envir=environment())
  oldcodes <- sapply(oldcodes,function(o) x %in% o)
  if(!is.matrix(oldcodes)) oldcodes <- t(oldcodes)
  nevtrue <- colSums(oldcodes) == 0
  if(any(nevtrue)){
    nrecng <- paste(recodings[nevtrue])
    if(length(nrecng)>1)
      warning("recodings ",paste(nrecng,collapse=", ")," have no consequences")
    else
      warning("recoding ",nrecng," has no consequences")
  }
  ambiguous <- rowSums(oldcodes) > 1
  if(any(ambiguous))
    stop("recoding request is ambiguous")

  y <- integer(length=length(x))
  for(i in 1:ncol(oldcodes)){
    y[oldcodes[,i]] <- i
  }
  if(!is.na(otherwise) && otherwise=="copy"){
    olevels <- levels(unique(x[y==0,drop=TRUE]))
    max.y <- max(y)
    for(i in seq(along=olevels)){
      y[x == olevels[i]] <- max.y + i
    }
    y <- factor(y,levels=1:max(y),labels=c(newcodes,olevels))
  }
  else if(is.character(otherwise)){
    max.y <- max(y)
    y[y == 0] <- max.y + 1
    y <- factor(y,levels=1:max(y),labels=c(newcodes,otherwise[1]))
  }
  else {
    y <- factor(y,levels=1:max(y),labels=newcodes)
  }
  return(y)
})

