
unarray <- function(x,fill=FALSE){
  x <- drop(x)
  if(!length(dim(x))) return(x)
  if(length(dim(x))==1 || !length(dimnames(x))) return(c(x))
  dims <- dim(x)
  dimn <- dimnames(x)
  for(i in seq_along(dimn)){
    if(!length(dimn[[i]]))
      if(fill)
        dimn[[i]] <- format(seq_len(dims[i]))
      else
        dimn[[i]] <- character(dims[i])
    }
  names <- c(mutils:::reduce(dimn,outer,dotpaste))
  structure(c(x),names=names)
}

array_to_matrix <- function(x,fill=FALSE){
  if(is.matrix(x)) return(x)
  x <- drop(x)
  n <- length(dim(x))
  dims <- dim(x)[-n]
  dimn <- dimnames(x)[-n]
  for(i in seq_along(dimn)){
    if(!length(dimn[[i]]))
      if(fill)
        dimn[[i]] <- format(seq_len(dims[i]))
      else
        dimn[[i]] <- character(dims[i])
    }
  dimn <- c(mutils:::reduce(dimn,outer,dotpaste))
  dim(x) <- c(prod(dims),dim(x)[n])
  rown <- if(length(dimnames(x)[n])) dimnames(x)[n] else list(NULL)
  dimnames(x) <- c(list(dimn),rown)
  x
}

dotpaste <- function(x,y)ifelse(nchar(x)&nchar(y),paste(x,y,sep="."),paste(x,y,sep=""))



has.response <- function(formula,data=NULL){
  #if(length(dim(data))) data <- data[1,]
  #as.logical(attr(terms(formula,data=data),"response"))
  length(formula) > 2
}


mk.ixes <- function(dims){
 ijk <- as.matrix(seq(dims[1]))
 if(length(dims)>1){
  for(i in 2:length(dims)){
    tmp.nrow <- nrow(ijk)
    ijk <- ijk[rep(seq(nrow(ijk)),dims[i]),]
    ijk <- cbind(ijk,
              rep(
                  seq(dims[i]),
                  rep(tmp.nrow,dims[i])
                  )
                )
  }
 }
 ijk
}


genTable <- function (formula,
                        data=parent.frame(),
                        subset=NULL,
                        labels=NULL,
                        addFreq=TRUE,
                        dot = NULL,
                        ...){
   m <- match.call()
   mis.data <- missing(data)
   formula <- try(as.formula(formula),silent=TRUE)

   fcall <- formula[[2]]

   if("." %in% all.vars(fcall)){

      if(length(dot)){
        allvars <- as.character(dot)
      }
      else {
        allvars <- names(data)
        if(!length(allvars) && is.environment(data))
          allvars <- ls(data)
      }

      allvars <- names(data)
      if(!length(allvars) && is.environment(data))
        allvars <- ls(data)

      if("." %nin% allvars){

        allvars <- setdiff(allvars,all.vars(formula[-2]))
        if(!length(allvars)) stop("'.' used both on the left and the right hand side of 'x' argument")
        data <- data[allvars]
        allvars <- lapply(allvars,as.symbol)
        ffcall <- lapply(allvars,function(x)Substitute(fcall,list("."=x)))
        fcall <- as.call(c(quote(c),ffcall))
        formula[[2]] <- fcall
        m$x <- formula
      }
   }

   m[[1]] <- as.name("fapply")
   names(m)[2] <- "formula"
   m$dot <- NULL
   m$labels <- NULL

   res <- eval(m, parent.frame())

   by <- attr(res,"by")

   if(is.list(res)){
    isArr <- sapply(res,is.array)
    if(all(isArr))
      res <- clct.arrays(res)
    else
      res <- clct.vectors(res)
    }
   if(has.response(formula)){
      formula <- formula[-2]
      }
   else
      fcall <- NULL
   if(length(fcall) > 1 && as.character(fcall[[1]]) %in% c("table","Table","percent","nvalid"))
        res[is.na(res)] <- 0
   if(length(dim(res)) == 2 && missing(labels)){
    if(is.null(rownames(res))){
      if(!missing(labels)) rownames(res) <- labels
      else if(nrow(res)==1) rownames(res) <- deparse(fcall)
      else if(length(fcall) > 1 &&  as.character(fcall[[1]]) %in% c("c","cbind","rbind")
              && length(fcall[-1]) == nrow(res))
            rownames(res) <- paste(fcall[-1])
      else if(length(fcall) > 1 && as.character(fcall[[1]]) %in% c("range"))
        rownames(res) <- c("Min","Max")
      else
        rownames(res) <- seq_len(nrow(res))
    } else if(!missing(labels)){
      colnames(res) <- labels
    }
   }
   by.dimnames <- lapply(by,function(x)as.character(sort(unique(x),na.last=TRUE)))
   by.dims <- sapply(by.dimnames,length)

   if(!length(dim(res))){

      tmp <- res
      res <- array(NA,by.dims)
      dimnames(res) <- by.dimnames

      ijk <- sapply(by,function(bby)
                  match(bby,sort(unique(bby),na.last=TRUE))
                  )

      res[ijk] <- tmp
      if(length(fcall) <= 1) res[is.na(res)] <- 0
      as.table(res)
   }
   else {

      res.dims <- dim(res)
      res.dimnames <- dimnames(res)
      res.dims <- res.dims[-length(res.dims)]
      res.dimnames <- res.dimnames[-length(res.dimnames)]

      ijk.res <- mk.ixes(res.dims)
      ijk.res <- ijk.res[rep(seq(nrow(ijk.res)),nrow(by)),,drop=FALSE]

      ijk.by <- sapply(by,function(bby)
                  rep(match(bby,sort(unique(bby),na.last=TRUE)),
                    rep(prod(res.dims), nrow(by))
                    )
                  )
      ijk <- cbind(ijk.res,ijk.by)

      tmp <- res
      res <- array(NA,c(res.dims,by.dims))
      res[ijk] <- tmp

      if(length(fcall) <= 1) res[is.na(res)] <- 0

      dimnames(res) <- c(res.dimnames,by.dimnames)

      as.table(drop(res))

    }
}

# aggregate.formula <- function (x,
#                         data = parent.frame(),
#                         subset = NULL,
#                         sort=TRUE,
#                         labels=NULL,
#                         addFreq=TRUE,
#                         as.vars=NA,
#                         drop.constants=TRUE,
#                         dot = NULL,
#                         ...)


Aggregate <- function (formula,
                        data = parent.frame(),
                        subset = NULL,
                        sort=TRUE,
                        labels=NULL,
                        addFreq=TRUE,
                        as.vars=NA,
                        drop.constants=TRUE,
                        dot = NULL,
                        ...)
{
   m <- match.call()
   mis.data <- missing(data)

   fcall <- formula[[2]]

   if("." %in% all.vars(fcall)){

      if(length(dot)){
        allvars <- as.character(dot)
      }
      else {
        allvars <- names(data)
        if(!length(allvars) && is.environment(data))
          allvars <- ls(data)
      }

      if("." %nin% allvars){

        allvars <- setdiff(allvars,all.vars(formula[-2]))
        if(!length(allvars)) stop("'.' used both on the left and the right hand side of 'x' argument")
        data <- data[allvars]
        allvars <- lapply(allvars,as.symbol)
        ffcall <- lapply(allvars,function(x)Substitute(fcall,list("."=x)))
        fcall <- as.call(c(quote(c),ffcall))
        formula[[2]] <- fcall
        m$formula <- formula
      }
   }

   m[[1]] <- as.name("fapply")
   names(m)[2] <- "formula"
   m$dot <- NULL
   m$labels <- NULL

   res <- eval(m, parent.frame())

   by <- attr(res,"by")
   attr(res,"by") <- NULL

   if(is.atomic(res) && !is.array(res)){
      res <- as.matrix(res)
      colnames(res) <- deparse(fcall)
      as.vars <- NULL
   }
   else if(is.list(res)){
    isArr <- sapply(res,is.array)
    if(!all(isArr)) as.vars <- NULL
    else if(is.na(as.vars)){
      diml <- sapply(res,function(x)length(dim(x)))
      as.vars <- max(diml)
    }
   }
   else if(is.matrix(res)) {

      if(is.character(as.vars)){
        as.vars <- match(as.vars,names(dimnames(res)),nomatch=0L)
        if(!as.vars) stop("undefined variable used for 'as.vars'")
      }
      if(is.na(as.vars))
        as.vars <- 1
   }
   else if(is.array(res)) {
      if(is.character(as.vars)){
        as.vars <- match(as.vars,names(dimnames(res)),nomatch=0L)
        if(!as.vars) stop("undefined variable used for 'as.vars'")
      }
      if(is.na(as.vars)){
        if(length(fcall) > 1 && as.character(fcall[[1]]) =="Table"){
          as.vars <- match("Statistics",names(dimnames(res)))
          if(is.na(as.vars)) as.vars <- 2
        }
        else
          as.vars <- 2
      }
   }
   else as.vars <- NULL

   if(has.response(formula))
      formula <- formula[-2]
   else {
      ii <- order(row.names(by))
      tmp <- by
      tmp[,"Freq"] <- as.vector(res)
      res <- tmp[ii,,drop=FALSE]
      return(res)
   }

   if(!length(as.vars)){

    if(is.list(res))
      res <- t(clct.vectors(lapply(res,unarray)))
    else if(is.array(res) && !is.matrix(res))
      res <- array_to_matrix(res)
    if(length(fcall) > 1 && as.character(fcall[[1]]) %in% c("table","Table","percent","nvalid"))
          res[is.na(res)] <- 0

    if(!length(colnames(res)) && missing(labels)){
      if(ncol(res)==1) colnames(res) <- deparse(fcall)
      else if(length(fcall) > 1 && as.character(fcall[[1]]) %in% c("c","cbind","rbind")
              && length(fcall[-1]) == ncol(res))
        colnames(res) <- paste(fcall[-1])
      else if(length(fcall) > 1 && as.character(fcall[[1]]) %in% c("range"))
        colnames(res) <- c("Min","Max")
      else
        colnames(res) <- seq_len(nrow(res))
    } else if(!missing(labels)){
      colnames(res) <- labels
    }

    if(sort)
      ii <- do.call("order",rev(by))
    else
      ii <- order(row.names(by))

    if(drop.constants){
        keep <- sapply(by,function(x)length(unique(x))>1)
        by <- by[,keep,drop=FALSE]
      }

    tmp <- by
    res <- if(isS4(by))as(res,class(by)) else as.data.frame(res,optional=TRUE)
    tmp[,names(res)] <- res
    res <- tmp[ii,,drop=FALSE]
   }
   else {

    if(sort)
      ii <- do.call("order",rev(by))
    else
      ii <- order(row.names(by))

    if(is.list(res)){
      res <- lapply(res[ii],to.data.frame,as.vars=as.vars)
      by <- by[ii,,drop=FALSE]
      rp <- sapply(res,nrow)
      ii <- seq_len(nrow(by))
      ii <- rep(ii,rp)
      by <- by[ii,,drop=FALSE]
      res <- do.call(rbind,res)
    }
    else if(is.array(res)){
      iii <- lapply(dim(res),seq_len)
      iii[[length(iii)]] <- ii
      res <- do.call("[",c(list(res),iii))
      by <- by[ii,,drop=FALSE]
      res <- to.data.frame(res,as.vars=as.vars)
      drp <- match("..by..",names(res))
      if(is.finite(drp))
        res <- res[-drp]
      rp <- nrow(res) %/% nrow(by)
      ii <- seq_len(nrow(by))
      ii <- rep(ii,each=rp)
      by <- by[ii,,drop=FALSE]
    }

    if(length(fcall) > 1 && as.character(fcall[[1]]) %in% c("table","Table","percent","nvalid"))
          res[is.na(res)] <- 0

    if(drop.constants){
        keep <- sapply(by,function(x)length(unique(x))>1)
        by <- by[,keep,drop=FALSE]
      }

    res <- if(isS4(by))as(res,class(by)) else as.data.frame(res)
    ncol.by <- ncol(by)
    ncol.res <- ncol(res)
    res <- cbind(by,res)
    cols.by <- 1:ncol.by
    cols.res <- ncol.by + 1:ncol.res
    by.names <- names(res)[cols.by]
    res.names <- names(res)[cols.res]
    
    if(all(grepl("^by[.]",by.names)))
      by.names <- gsub("^by[.]","",by.names)
    if(all(grepl("^res[.]",res.names)))
      res.names <- gsub("^res[.]","",res.names)
    names(res) <- c(by.names,res.names)
   }
   res
}
