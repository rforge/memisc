setMethod("as.data.set","importer",function(x,row.names=NULL,optional=NULL,...){
  rewind(x)
  res <- readData(x,n=nrow(x))
  names(res) <- names(x)
  class(res) <- "data.frame"
  if(length(row.names)){
    if(length(row.names)!=nrow(x)) stop("row.names argument has wrong length")
    attr(res,"row.names") <- row.names
  }
  else
    attr(res,"row.names") <- seq_len(nrow(x))
  new("data.set",res)
})


setMethod("dim","importer",function(x){
  c(getNobs(x),length(x@.Data))
})

# setMethod("names","importer",function(x)names(x@frame))


bracket.importer <- function(x,Nargs,i,j,drop=TRUE,...){
  mycall <- match.call()
  mdrp <- missing(drop)
  #Nargs <- Nargs - (!mdrp) - (!mspl) - length(list(...))
  
  ncases <- nrow(x)
  names <- names(x)
  nvar <- length(names)

#   browser()
#  message("Nargs = ",Nargs)

  if(Nargs == 1){
    if(missing(i)) return(as.data.set(x))
    if(is.array(i)) stop("index arrays not supported")
    j <- i
    i <- rep(TRUE,ncases)
  }
  else{
    if(missing(i)){
      i <- rep(TRUE,ncases)
    }
    if(missing(j)){
      j <- rep(TRUE,nvar)
    }
  }
  
  if(is.logical(i)){
    rows <- i
    ui <- which(i)
  } else if(is.numeric(i)){
    i <- as.integer(i)
    ui <- sort(unique(i))
    rows <- logical(max(ui))
    rows[ui] <- TRUE
  }
  else stop("mode ",sQuote(mode(i))," not supported for row selection")

  if(is.logical(j)){
    cols <- j
  } else if(is.numeric(j)){
    if(max(j)>nvar) stop("undefined columns selected")
    cols <- logical(nvar)
    cols[j] <- TRUE
  } else if(is.character(j)){
    if(!all(j %in% names)) stop("undefined columns selected")
    cols <- names %in% j
  } else stop("mode ",sQuote(mode(i))," not supported for column selection")
  
  rewind(x)
  if(length(rows) > nrow(x)) length(rows) <- nrow(x)
  res <- readSubset(x,rows,cols)
  names(res) <- names[cols]
  attr(res,"row.names") <- ui
  class(res) <- "data.frame"
  if(is.numeric(i)){
    ii <- match(i,ui)
    res <- res[ii,,drop=FALSE]
  }
  if(drop && length(res)==1) res[[1]]
  else new("data.set",res)
}

setMethod("[",signature(x="importer",i="atomic",j="atomic",drop="ANY"),
  function(x,i,j,...,drop=TRUE)
    bracket.importer(x,Nargs=nargs()-1-(!missing(drop))-length(list(...)),i,j,drop=drop,...)
)
setMethod("[",signature(x="importer",i="atomic",j="missing",drop="ANY"),
  function(x,i,j,...,drop=TRUE)
    bracket.importer(x,Nargs=nargs()-1-(!missing(drop))-length(list(...)),i,j,drop=drop,...)
)
setMethod("[",signature(x="importer",i="missing",j="atomic",drop="ANY"),
  function(x,i,j,...,drop=TRUE)
    bracket.importer(x,Nargs=nargs()-1-(!missing(drop))-length(list(...)),i,j,drop=drop,...)
)
setMethod("[",signature(x="importer",i="missing",j="missing",drop="ANY"),
  function(x,i,j,...,drop=TRUE)
    bracket.importer(x,Nargs=nargs()-1-(!missing(drop))-length(list(...)),i,j,drop=drop,...)
)


setMethod("subset","importer",
  function (x, subset, select, drop = FALSE, ...){
    ncases <- nrow(x)
    names <- names(x)
    nvars <- length(names)

    if (missing(subset))
        r <- rep(TRUE,ncases)
    else {
        e <- substitute(subset)
        subset.vars <- all.vars(e)
        r.s <- rep(TRUE,ncases)
        j.s <- names %in% subset.vars
        r <- NA
        r <- eval(e, x[r.s,j.s,drop=FALSE], parent.frame())
        if (!is.logical(r))
            stop("'subset' must evaluate to logical")
        r <- r & !is.na(r)
    }
    if (missing(select)) x[r,, drop = drop,...]
    else {
        nl <- as.list(1:nvars)
        names(nl) <- names
        use <- logical(nvars)
        use[eval(substitute(select), nl, parent.frame())] <- TRUE
        y <- x[r, use, drop = drop,...]
        vars <- sapply(substitute(select)[-1],as.character)
        ii <- match(vars,names[use])
        y <- y[ii]
        new.names <- names(vars)
        if(length(new.names) && any(nzchar(new.names))){
          names.y <- names(y)
          names.y[nzchar(new.names)] <- new.names[nzchar(new.names)]
          names(y) <- names.y
        }
        y
   }   
})


setMethod("description","importer",function(x){
  res <- lapply(x,description)
  res <- sapply(res,function(des){
          if(length(des)) sQuote(des)
          else " (none) "
          })
  structure(res,class="descriptions")
})

setMethod("codebook","importer",function(x){

   cs <- getOption("codebook.chunk.size")
   ts <- prod(dim(x))
   nc <- ncol(x)

   nn <- if(ts > cs) ts %/% cs else nc
   message(nn," variables per chunk")

   m <- nc%/%nn
   r <- nc%%nn
   ii <- seq(1:m)

   cb <- sapply(ii,function(i){
         jj <- seq(from=(i-1)*nn+1,to=i*nn)
         xn <- x[jj]
         codebook(xn)
      })
   if(r > 0){
         jj <- seq(from=m*nn+1,to=nc)
         cb <- c(cb,list(codebook(x[jj])))
   }
   cb <- unlist(cb,recursive=FALSE)
   names(cb) <- names(x)
   new("codebook",cb)
})

