setMethod("recode","item",function(x,...,otherwise=NA){
  recodings <- match.call(expand.dots=FALSE)$...
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
      min.list <- list(min=min(x))
      oldcodes[has.range][has.min] <- lapply(
                oldcodes[has.range][has.min],
                function(x) do.call("substitute",list(x,min.list))
                )
    }
    if(any(has.max)){
      max.list <- list(max=max(x))
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
  torecode <- sapply(oldcodes,eval,envir=environment(),enclos=parent.frame())
  if(!is.matrix(torecode)) torecode <- t(torecode)
  newcodes <- sapply(newcodes,eval,parent.frame())
  nevtrue <- colSums(torecode) == 0
  if(any(nevtrue)){
    nrecng <- paste(recodings[nevtrue])
    if(length(nrecng)>1)
      message("Note: Recodings ",paste(nrecng,collapse=", ")," have no consequences")
    else
      message("Note: Recoding ",nrecng," has no consequences")
  }
  ambiguous <- rowSums(torecode) > 1
  if(any(ambiguous))
    stop("recoding request is ambiguous")
  #y <- if(is.character(newcodes)) as.character(x) else x
  y <- x
  labels(y) <- NULL
  missing.values(y) <- NULL
  if(is.character(newcodes) && is.numeric(y)) {
    newcodes <- structure(
      seq_along(newcodes),
      names=newcodes
      )
  }
  newcodes <- newcodes[!nevtrue]
  torecode <- torecode[,!nevtrue,drop=FALSE]
  for(i in seq(along=newcodes)){
    y[torecode[,i]] <- as.vector(newcodes[i],mode=storage.mode(y))
  }
  if(!identical(otherwise,"copy")){
    recoded <- as.logical(rowSums(torecode))
    tmp <- as.vector(otherwise,mode=storage.mode(y))
    length(otherwise) <- length(y)
    otherwise[] <- tmp
    y[!recoded] <- otherwise[!recoded]
  }
  newvlab <- newcodes[nzchar(names(newcodes))]

  if(length(lab.y <- labels(x)) && identical(otherwise,"copy")){
    lab.y.val <- lab.y@values
    lab.oldcodes <- lapply(oldcodes,Substitute,list(x=lab.y.val))
    lab.torecode <- sapply(lab.oldcodes,eval,envir=environment(),enclos=parent.frame())
    for(i in seq(along=newcodes)){
      lab.y.val[lab.torecode[,i]] <- as.vector(newcodes[i],mode=storage.mode(lab.y.val))
    }
    lab.y.val <- sort(unique(lab.y.val))
    ii <- match(lab.y.val,lab.y@values)
    lab.y.lab <- lab.y@.Data[ii]
    labels(y) <- new("value.labels",lab.y.lab,values=lab.y.val)
    if(length(newvlab))
      labels(y) <- labels(y) + newvlab
  }
  else if(length(newvlab)){
    if(length(names(otherwise)))
      newvlab <- new("value.labels",names(newvlab),values=newvlab) + otherwise
    labels(y) <- newvlab
  }
#   else if(length(newvlab)){
#     if(identical(otherwise,"copy"))
#       labels(y) <- labels(y) + newvlab
#     else
#       labels(y) <- newvlab
#     }

  return(y)
})

