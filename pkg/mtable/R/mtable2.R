mtable2 <- function(...,
                   list=NULL,
                   coef.style=getOption("coef.style"),
                   summary.stats=TRUE,
                   factor.style=getOption("factor.style"),
                   baselevel.sep=getOption("baselevel.sep"),
                   getSummary=eval.parent(quote(getSummary)),
                   float.style=getOption("float.style"),
                   digits=min(3,getOption("digits")),
                   signif.symbols=getOption("signif.symbols"),
                   drop=TRUE,
                   gs.args=NULL,
                   relabel=NULL,
                   intercept.pos=getOption("intercept.pos","top")
){

  intercept.pos <- match.arg(intercept.pos,c("top","bottom"))
  
  args <- list(...)
  if(length(args)==1 && inherits(args[[1]],"by"))
    args <- args[[1]]
  if(length(args)){

    argnames <- names(args)
    if(!length(argnames)) {
      m<-match.call(expand.dots=FALSE)
      argnames <- sapply(m$...,paste)
      names(args) <- argnames
    }
    
    if(any(sapply(args,inherits,"by"))){
      
      x <- list()
      for(i in seq_along(args)){
        
        if(inherits(args[[i]],"by"))
          x <- c(x,args[[i]])
        else 
          x <- c(x,args[i])
      }
    }
    else x <- args
  }
  
  if(length(list)){
    
    if(is.list(list)){
      
      x <- c(x,list)
    }
    else stop("'list' argument should be a list")
  }

  x.classes <- lapply(x,class)
  if(any(sapply(x.classes,length))==0) stop("don\'t know how to handle these arguments")
  
  if(!length(gs.args))
    summaries <- lapply(x,getSummary)
  else if(all(names(gs.args)==names(x))){
    summaries <- structure(vector("list",length=length(x)),
                  names=names(x))
    for(i in 1:length(x))
      summaries[[i]] <- eval(as.call(
        c(list(as.name("lapply"),
               as.name("x"),
               FUN=as.name("getSummary")),
          gs.args[[i]]
        )))
  }
  else {
    summaries <- eval(as.call(
      c(list(as.name("lapply"),
             as.name("x"),
             FUN=as.name("getSummary")),
        gs.args
      )))
  } 
  
  calls <- lapply(summaries,"[[","call")
  
  template <- as.matrix(getCoefTemplate(coef.style))
  tdims <- dim(template)
  ltdims <- length(tdims)
  if(ltdims>2) stop("can\'t handle templates with dim>2")
  
  parms <- lapply(summaries,getParams)

  fparms <- lapply(parms,
                   formatParams,
                   template=template,
                   factor.style=factor.style,
                   baselevel.sep=baselevel.sep,
                   float.style=float.style,
                   digits=digits,
                   signif.symbols=signif.symbols,
                   relabel=relabel)
  fparms <- arrangeParms(fparms)
  #fparms <- apply(fparms,1,match_arr,along=3,fill="")

  interceptLabel <- "(Intercept)"
  
  for(i in 1:nrow(fparms))
    fparms[i,] <- match_arr(fparms[i,],along=3,fill="")

  for(j in 1:ncol(fparms))
    fparms[,j] <- xpd_arr(fparms[,j],along=4,fill="")

  for(i in 1:nrow(fparms))
    fparms[i,] <- putIntercept(fparms[i,],intercept.pos,interceptLabel)
    
  if(isTRUE(summary.stats))
    select.sumstats <- lapply(x,getDefaultSumStat)
  else if(is.list(summary.stats)){
    if(!length(summary.stats)!=length(x))
      stop("'summary.statistics' argument has wrong length")
    if(!all(sapply(summary.stats,is.character)))
      stop("If 'summary.statistics' argument is a list, its elements must be character strings")
    select.sumstats <- summary.stats
  } else if(is.character(summary.stats)){
    select.sumstats <- rep(list(summary.stats),
                              length(x))
  } else if(!is.logical(summary.stats))
    stop("cannot handle 'summary.stats' of mode",mode(summary.stats))

  fsumstats <- mapply(formatSumstats,x,summaries,select.sumstats,
         MoreArgs=list(digits=digits,
                       signif.symbols=signif.symbols),
         SIMPLIFY = FALSE)
  
  structure(
  list(params=fparms,
       summary.stats=fsumstats,
       model.calls=calls),
  class="mtable2")
}  

getParams <- function(smry){
  
  if(length(smry$params)){
    
    params <- smry$params
    if(!is.list(params))
      stop("'params' component of list returned by 'getSummary' should be a list")
    if(length(smry$coef)) {
      if (length(params$coef))
        stop("duplicate 'coef' component")
      params$coef <- smry$coef
    }
  }
  else {
    
    params <- list(coef=smry$coef)
  }
  
  structure(params,
       contrasts=smry$contrasts,
       xlevels=smry$xlevels)
}

formatParams <- function(params,
                         template,
                         factor.style,
                         baselevel.sep,
                         float.style,
                         digits,
                         signif.symbols,
                         relabel){
  lapply(params,
         formatParams1,
         contrasts=attr(params,"contrasts"),
         xlevels=attr(params,"xlevels"),
         template=template,
         factor.style=factor.style,
         baselevel.sep=baselevel.sep,
         float.style=float.style,
         digits=digits,
         signif.symbols=signif.symbols,
         relabel=relabel)
}

formatParams1 <- function(params,
                          contrasts,
                          xlevels,
                          template,
                          factor.style,
                          baselevel.sep,
                          float.style,
                          digits,
                          signif.symbols,
                          relabel){
  
  if(length(relabel)){
    for(d in seq_along(dim(params))){
      cargs <- c(list(params,dim=d),relabel)
      if(!length(relabel.arg$warn)) relabel.arg$warn <- FALSE
      coef <- do.call(dimrename,relabel.arg)
    }
  }
  dimnames(params)[[1]] <- prettyNames(dimnames(params)[[1]],
                                       contrasts=contrasts,
                                       xlevels=xlevels,
                                       factor.style=factor.style,
                                       baselevel.sep)
  adims <- if(length(dim(params))==2) 1 else c(1,3)
  ans <- apply(params,
               adims,
               applyTemplate,
               template=template,
               float.style=float.style,
               digits=digits,
               signif.symbols=signif.symbols)
  
  if(length(dim(template))){
    
    template <- dummy.dimnames(template,style="empty")
    newdims <- c(dim(template),dim(ans)[-1])
    newdimnames <- c(dimnames(template),dimnames(ans)[-1])
    dim(ans) <- newdims
    dimnames(ans) <- newdimnames
  } else rownames(ans) <- names(template)
  
  ans[ans=="()"] <- ""
  
  return(ans)
}

arrangeParms <- function(parms){

  coln <- names(parms)
  rown <- unique(unlist(lapply(parms,names)))
  m <- length(rown)
  n <- length(parms)
  
  res <- matrix(list(),m,n,
            dimnames=list(rown,coln))
  for(i in 1:n)
    res[names(parms[[i]]),i] <- parms[[i]]
  res
}

putIntercept <- function(x,whereTo,interceptLabel){

  coefnames <- dimnames(x[[1]])[[3]]
  numcoef <- dim(x[[1]])[3]
  ipos <- match(interceptLabel,coefnames,0L)

  if(ipos){

    ii <- 1:numcoef
    if(whereTo=="top")
      ii <- c(ipos,ii[-ipos])
    else
      ii <- c(ii[-ipos],ipos)
    for(i in 1:length(x))
      x[[i]] <- x[[i]][,,ii,,drop=FALSE]
  }
  x
}

formatSumstats <- function(x,smry,select,
                        digits=digits,
                        signif.symbols=signif.symbols){
  template <- getSummaryTemplate(x)
  sumstats <- smry$sumstat
  sumstats <- drop(applyTemplate(sumstats,
       template,
       digits=digits,
       signif.symbols=signif.symbols))
  
  if(length(select)){

    if(!all(select %in% names(sumstats))){
      undefnd <- summary.stats[!(select %in% rownames(sumstats))]
      undefnd <- paste(sQuote(undefnd),sep=", ")
      if(length(undefnd)==1)
        stop("summary statistic ",undefnd," is undefined")
      else
        stop("summary statistics ",undefnd," are undefined")
    }
    sumstats[select]
  }
  else sumstats
}

getDefaultSumStat <- function(obj){
  classes <- class(obj)
  for(i in 1:length(classes)){
    
    cls <- classes[i]
    sumstatOpt <- getOption(paste("summary.stats",cls,sep="."))
    if(length(sumstatOpt)) return(sumstatOpt)
  }
  NULL
}

cbindli <- function(x,i) do.call(cbind,x[i,])
rbindli <- function(x,i) do.call(rbind,x[,i])

getLeaders <- function(x)x[-(1:3),1]
getHeaders <- function(x)x[1,-(1:2)]
mkCmpct <- function(x)x[-(1:3),-(1:2)]

format.mtable2 <- function(x,...){

  fparms <- lapply(x$params,ftable,
                   row.vars=c(3,1),
                   col.vars=c(4,2))
  fparms <- array(lapply(fparms,format,...),
                  dim=dim(x$params),
                  dimnames=dimnames(x$params)
                  )
  leaders <- lapply(fparms[,1],getLeaders)
  headers <- lapply(fparms[1,],getHeaders)

  leaders <- unlist(leaders)
  headers <- unlist(headers)
  
  body <- array(lapply(fparms,mkCmpct),
                  dim=dim(x$params),
                  dimnames=dimnames(x$params)
                  )
  
  body <- lapply(1:ncol(body),rbindli,x=body)
  body <- do.call(cbind,body)

  out <- cbind(leaders,body)
  out <- rbind(c("",headers),out)
  out <- apply(out,2,format,qoute=FALSE)
  out
}

print.mtable2 <- function(x,...){

  r <- format.mtable2(x,quote=FALSE,...)
  r <- apply(r,1,paste,collapse=" ")
  cat(r,sep="\n")
  invisible(x)
}