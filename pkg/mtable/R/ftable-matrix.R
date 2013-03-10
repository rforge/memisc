cbind.ftable <- function(..., deparse.level=1){
  
  args <- list(...)
  argnames <- names(args)
  rv <- lapply(args,attr,"row.vars")
  rv.ok <- TRUE
  narg <- length(args)
  if(length(args)>1){
    for(i in 1:narg){
      if(!length(rv[[i]])){
        if(i==1) stop("first argument must by an ftable")
        if(is.matrix(args[[i]])){
          if(nrow(args[[i]])!=nrow(args[[1]])) stop("row vars must be identical")
          tmp <- args[[i]]
        }
        else{
          tmp <- matrix(NA,nrow=nrow(args[[1]]),ncol=1)
          tmp[] <- as.numeric(args[[i]])
          colnames(tmp) <- if(length(argnames)&&length(argnames[[i]]))argnames[[i]] else ""
          names(dimnames(tmp))[2] <- ""
        }
        attr(tmp,"row.vars") <- rv[[1]]
        attr(tmp,"col.vars") <- dimnames(tmp)[2]
        dimnames(tmp) <- NULL
        class(tmp) <- "ftable"
        args[[i]] <- tmp
      }
      else if(!identical(rv[[1]],rv[[i]])) {
        rv.ok <- FALSE
        break
      }
    }
  }

  if(!(rv.ok)) stop("row vars must be identical")

  row.vars <- list(attr(args[[1]],"row.vars"))
  col.vars <- lapply(args,attr,"col.vars")
  names(row.vars) <- NULL
  names(col.vars) <- NULL
  args <- lapply(args,function(arg)array(arg,dim=dim(arg)))
  res <- matrix(args,nrow=1,ncol=length(args))
  structure(res,
    row.vars=row.vars,
    col.vars=col.vars,
    class="ftable_matrix")
}

rbind.ftable <- function(..., deparse.level=1){

  args <- list(...)
  argnames <- names(args)
  cv <- lapply(args,attr,"col.vars")
  cv.ok <- TRUE
  narg <- length(args)
  if(length(args)>1){
    for(i in 1:narg){
      if(!length(cv[[i]])){
        if(i==1) stop("first argument must by an ftable")
        if(is.matrix(args[[i]])){
          if(ncol(args[[i]])!=ncol(args[[1]])) stop("col vars must be identical")
          tmp <- args[[i]]
        }
        else{
          tmp <- matrix(NA,ncol=ncol(args[[1]]),nrow=1)
          tmp[] <- as.numeric(args[[i]])
          rownames(tmp) <- if(length(argnames)&&length(argnames[[i]]))argnames[[i]] else ""
          names(dimnames(tmp))[1] <- ""
#           browser()
        }
        attr(tmp,"row.vars") <- dimnames(tmp)[1]
        attr(tmp,"col.vars") <- cv[[1]]
        dimnames(tmp) <- NULL
        class(tmp) <- "ftable"
        args[[i]] <- tmp
      }
      else if(!identical(cv[[1]],cv[[i]])) {
        cv.ok <- FALSE
        break
      }
    }
  }

  if(!(cv.ok)) stop("column vars must be identical")

  col.vars <- list(attr(args[[1]],"col.vars"))
  row.vars <- lapply(args,attr,"row.vars")
  names(row.vars) <- NULL
  names(col.vars) <- NULL
  args <- lapply(args,function(arg)array(arg,dim=dim(arg)))
  res <- matrix(args,ncol=1,nrow=length(args))
  structure(res,
    row.vars=row.vars,
    col.vars=col.vars,
    class="ftable_matrix")
}

cbind.ftable_matrix <- function(...){

  args <- list(...)
  inh.fm <- sapply(args,inherits,"ftable_matrix")
  classes.ok <- all(inh.fm)
  if(!classes.ok) stop("all arguments must be 'ftable' or 'ftable_matrix' objects")

  rv <- lapply(args,attr,"row.vars")
  rv.ok <- TRUE
  narg <- length(args)
  if(length(args)>1){
    for(i in 1:narg){
      if(!identical(rv[[1]],rv[[i]])) {
        rv.ok <- FALSE
        break
      }
    }
  }

  if(!(rv.ok)) stop("row vars must be identical")

  row.vars <- rv[[1]]
  col.vars <- do.call(c,lapply(args,attr,"col.vars"))
  res <- lapply(args,function(x){
                attributes(x)[c("col.vars","row.vars","class")] <- NULL
                x
              })
  res <- do.call(cbind,res)
  structure(res,
    row.vars=row.vars,
    col.vars=col.vars,
    class="ftable_matrix")
}

rbind.ftable_matrix <- function(...){

  args <- list(...)
  inh.fm <- sapply(args,inherits,"ftable_matrix")
  classes.ok <- all(inh.fm)
  if(!classes.ok) stop("all arguments must be 'ftable' or 'ftable_matrix' objects")

  cv <- lapply(args,attr,"col.vars")
  cv.ok <- TRUE
  narg <- length(args)
  if(length(args)>1){
    for(i in 1:narg){
      if(!identical(cv[[1]],cv[[i]])) {
        cv.ok <- FALSE
        break
      }
    }
  }

  if(!(cv.ok)) stop("column vars must be identical")

  row.vars <- do.call(c,lapply(args,attr,"row.vars"))
  col.vars <- cv[[1]]
  res <- lapply(args,function(x){
                attributes(x)[c("col.vars","row.vars","class")] <- NULL
                x
              })
  res <- do.call(rbind,res)
  structure(res,
    row.vars=row.vars,
    col.vars=col.vars,
    class="ftable_matrix")
}

fm_mkhdr <- function(cv,quote){

  nms <- names(cv)
  l <- sapply(cv,length)
  prod.l <- cumprod(l)
  m <- length(cv)
  n <- prod.l[length(prod.l)]
  
  header <- matrix("",nrow=m,ncol=n)

  repn <- c(1,prod.l[-length(prod.l)])
  for(i in 1:m){

    ll <- prod.l[i]
    ii <- seq(from=1,length=ll,by=n%/%ll)
    header[i,ii] <- if(quote) paste0("\"",cv[[i]],"\"") else cv[[i]]
  }
  
  cbind(format(nms,justify="l"),header,"")
}

fm_mkldr <- function(rv,quote){

  nms <- names(rv)
  l <- sapply(rv,length)
  prod.l <- cumprod(l)
  m <- length(rv)
  n <- prod.l[length(prod.l)]

  header <- matrix("",nrow=n,ncol=m)

  repn <- c(1,prod.l[-length(prod.l)])
  for(i in 1:m){

    ll <- prod.l[i]
    ii <- seq(from=1,length=ll,by=n%/%ll)
    header[ii,i] <- if(quote) paste0("\"",rv[[i]],"\"") else rv[[i]]
  }

  structure(rbind(nms,header,""),dimnames=NULL)
}

to.nrow <- function(x,nrow,from=c("top","bottom")){

  from <- match.arg(from)
  res <- matrix(vector(mode=mode(x),length=length(x)),nrow=nrow,ncol=ncol(x))

  if(from=="top") res[1:nrow(x),] <- x
  else res[1:nrow(x) + nrow-nrow(x),] <- x
  res
}

to.ncol <- function(x,ncol,from=c("left","right")){

  from <- match.arg(from)
  res <- matrix(vector(mode=mode(x),length=length(x)),nrow=nrow(x),ncol=ncol)
  if(from=="left") res[,1:ncol(x)] <- x
  else res[,1:ncol(x) + ncol-ncol(x)] <- x
  res
}


format.ftable_matrix <- function(x,quote=TRUE,digits=0,format="f",...){

  d <- digits
  digits <- integer(ncol(x))
  digits[] <- d

  f <- format
  format <- character(ncol(x))
  format[] <- f

  fmt <- array(list(),dim=dim(x))
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){

      tmp <- formatC(x[[i,j]],digits=digits[j],format=format[j])
      tmp <- cbind("",tmp,"")
      tmp <- rbind("",tmp,"")
      fmt[[i,j]] <- tmp
    }
  }

  col.vars <- attr(x,"col.vars")
  row.vars <- attr(x,"row.vars")

  header <- lapply(col.vars,fm_mkhdr,quote=quote)
  max.hh <- max(sapply(header,nrow))

  header <- lapply(header,to.nrow,nrow=max.hh,from="bottom")
  header <- do.call(cbind,header)

  leader <- lapply(row.vars,fm_mkldr,quote=quote)
  max.lh <- max(sapply(leader,ncol))
  leader <- lapply(leader,to.ncol,ncol=max.lh,from="right")
  leader <- do.call(rbind,leader)

  leader <- rbind(matrix("",nrow=nrow(header),ncol=ncol(leader)),leader)
  leader <- apply(leader,2,format.default,justify="l")
  
  body <- list()
  for(i in 1:nrow(fmt))
    body[[i]] <- do.call(cbind,fmt[i,])
  body <- do.call(rbind,body)

  #body <- apply(body,2,format.default,justify="r")
  res <- rbind(header,body)
  res <- apply(res,2,format.default,justify="r")
  res <- cbind(leader,res)
  res
}

write.ftable_matrix <- function(x,
                            file = "",
                            quote = TRUE,
                            append = FALSE,
                            digits = 0){
  r <- format.ftable_matrix(x,quote=quote,digits=digits)
  r <- apply(r,1,paste,collapse=" ")
  cat(r, file = file, append = append, sep = "\n")
  invisible(x)
}

print.ftable_matrix <- function(x,quote=FALSE,...)
  write.ftable_matrix(x,file="",quote=quote,...)

.last <- function(x) x[[length(x)]]
.getElts <- function(x,i) lapply(x,"[[",i)
  
toLatex.ftable_matrix <- function(object,
          show.titles=TRUE,
          digits=0,
          format="f",
          useDcolumn=TRUE,
          colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "r",
          LaTeXdec=".",
          ddigits=max(1,digits),
          useBooktabs=TRUE,
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline\n",
          cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          header.varnames=TRUE,
          compact=FALSE,
          varontop,varinfront,
          groupsep="3pt",
          grouprule=midrule,
          ...)
{

    row.vars <- attr(object,"row.vars")
    col.vars <- attr(object,"col.vars")

    N <- nrow(object)
    M <- ncol(object)
    
    n <- sapply(object[,1],nrow)
    m <- sapply(object[1,],ncol)
    n.row.vars <- sapply(row.vars,length)
    n.col.vars <- sapply(col.vars,length)
    max.n.row.vars <- max(n.row.vars)
    max.n.col.vars <- max(n.col.vars)
    
    if(missing(varontop)) varontop <- max(n.col.vars) <= 1
    if(missing(varinfront)) varinfront <- max(n.row.vars) <= 1

    d <- digits
    digits <- integer(M)
    digits[] <- d

    fo <- format
    format <- integer(M)
    format[] <- fo

    csp <- colspec
    colspec <- character(length(m))
    colspec[] <- csp

    cv.desc <- sapply(col.vars,cv_get_desc,compact=compact)
    start.g <- cv.desc["start.g",]
    end.g <- cv.desc["end.g",]
    nms.cv <- cv.desc["nms.cv",]
    mcols <- cv.desc["mcols",]
    width <- cv.desc["width",]
    ii <- cv.desc["ii",]
    cv.repg <- cv.desc["cv.repg",]

    headers <- mapply(ltfm_mkHeader,col.vars,n.col.vars,mcols,width,cv.repg,ii,varontop,compact,SIMPLIFY=FALSE)
    
    l.hd <- sapply(headers,length)
    max.l.hd <- max(l.hd)

    for(i in 1:M){

      jj <- seq.int(to=max.l.hd,length.out=l.hd[i])
      tmp.hdr1 <- paste0(rep("",width[[i]]),collapse="&")
      if(n.col.vars[i]>1||!varontop & any(nzchar(nms.cv[[i]])) )
        tmp.hdr1 <- paste0("&",tmp.hdr1)
      
      tmp.header <- vector("list",max.l.hd)
      tmp.header[] <- list(tmp.hdr1)
      tmp.header[jj] <- headers[[i]]
      headers[[i]] <- tmp.header
    }

    headers <- lapply(1:max.l.hd,.getElts,x=headers)
    headers <- lapply(headers,unlist)

    for(i in 1:M){

      if(i==1){
        start.g[[i]] <- mapply(`+`,start.g[[i]],max.n.row.vars,SIMPLIFY=FALSE)
        end.g[[i]] <- mapply(`+`,end.g[[i]],max.n.row.vars,SIMPLIFY=FALSE)
      }
      else if(compact){
        start.g[[i]] <- mapply(`+`,start.g[[i]],tmp.end.g,SIMPLIFY=FALSE)
        end.g[[i]] <- mapply(`+`,end.g[[i]],tmp.end.g,SIMPLIFY=FALSE)
      }
      else {
        start.g[[i]] <- mapply(`+`,start.g[[i]],tmp.end.g+1,SIMPLIFY=FALSE)
        end.g[[i]] <- mapply(`+`,end.g[[i]],tmp.end.g+1,SIMPLIFY=FALSE)
      }
      
      if(n.col.vars[[i]] > 1 | (!varontop & nzchar(nms.cv[[i]]))[1]){
        start.g[[i]] <- mapply(`+`,start.g[[i]],1,SIMPLIFY=FALSE)
        end.g[[i]] <- mapply(`+`,end.g[[i]],1,SIMPLIFY=FALSE)
      }
      tmp.end.g <- .last(end.g[[i]])
      tmp.end.g <- .last(tmp.end.g)
    }

    for(i in 1:M){

#       jj <- seq.int(to=max.n.col.vars,
#                     length.out=if(varontop) n.col.vars[i] else n.col.vars[i]-1)
      jj <- seq.int(to=max.n.col.vars,length.out=n.col.vars[i])

      tmp.start.g <- vector("list",max.n.col.vars)
      if(varontop || n.col.vars[i]>1)
        tmp.start.g[jj] <- start.g[[i]]
      start.g[[i]] <- tmp.start.g
      tmp.end.g <- vector("list",max.n.col.vars)
      if(varontop || n.col.vars[i]>1)
        tmp.end.g[jj] <- end.g[[i]]
      end.g[[i]] <- tmp.end.g
    }
    
    start.g <- lapply(1:max.n.col.vars,.getElts,x=start.g)
    end.g <- lapply(1:max.n.col.vars,.getElts,x=end.g)
    start.g <- lapply(start.g,unlist)
    end.g <- lapply(end.g,unlist)

    if(varinfront) {
    
      start.g <- mapply(`+`,start.g,1)
      end.g <- mapply(`+`,end.g,1)
    }
# if(N>1)browser()
    
    header <- unlist(lapply(headers,paste0,collapse=if(compact)"&" else "&&"))
    len.hdr <- length(header)
    #header <- paste0(paste0(rep("&",max.n.row.vars),collapse=""),header)
    lheader <- matrix("",ncol=max.n.row.vars,nrow=len.hdr)
    lheader[len.hdr,seq.int(to=max.n.row.vars,length.out=n.row.vars[1])] <- names(row.vars[[1]])
    if(varinfront) lheader[,1] <- ""
    lheader <- apply(lheader,1,paste0,collapse="&")
    header <- paste(lheader,header,sep="&")
    if(varinfront)
      header <- paste0("&",header)
    
    cln <- mapply(mkclines,cmidrule,start.g,end.g)
    cln <- sapply(cln,paste0,collapse="")

#     header[-length(header)] <- paste0(header[-length(header)],rowsep,cln[-length(header)])
    
    header <- paste0(header,"\\\\")
#     browser()
    if(varontop)
      header[-1] <- paste0(cln,"\n",header[-1])
    else
      header[-1] <- paste0(cln[-1],"\n",header[-1])

    leaders <- mapply(ltfm_mkLeader,row.vars,n.row.vars,n,SIMPLIFY=FALSE)

    if(varinfront){

      for(i in 1:N){

        nm.row.vars.i <- names(row.vars[[i]])
        tmp.leaders <- matrix("",nrow=n[i],ncol=max.n.row.vars+1)
        jj <- seq.int(to=max.n.row.vars+1,length.out=n.row.vars[i])
        tmp.leaders[,jj] <- leaders[[i]]
        if(nzchar(nm.row.vars.i[1]))
          tmp.leaders[1,1] <- nm.row.vars.i[1]
        else if(varinfront && length(n[i] < 2)){
          tmp.leaders[1,1] <- tmp.leaders[1,max.n.row.vars+1]
          tmp.leaders[1,max.n.row.vars+1] <- ""
        }
        leaders[[i]] <- tmp.leaders
        if(i > 1 && n.row.vars[i] > 1 && any(nzchar(nm.row.vars.i[-n.row.vars[i]]))){
        
          tmp.leaders <- character(max.n.row.vars)
          tmp.leaders[jj] <- names(row.vars[[i]])
          leaders[[i]] <- rbind(tmp.leaders,leaders[[i]])
          }
      }
    }
    else {

      for(i in 1:N){

        tmp.leaders <- matrix("",nrow=n[i],ncol=max.n.row.vars)
        jj <- seq.int(to=max.n.row.vars,length.out=n.row.vars[i])
        tmp.leaders[,jj] <- leaders[[i]]
        leaders[[i]] <- tmp.leaders
        if(i > 1 && any(nzchar(names(row.vars[[i]])))
          ){
          tmp.leaders <- character(max.n.row.vars)
          tmp.leaders[jj] <- names(row.vars[[i]])
          leaders[[i]] <- rbind(tmp.leaders,leaders[[i]])
          }
      }
    }

    body <- array(list(),dim=dim(object))

    for(j in 1:M){
      for(i in 1:N){

        tmp.bdy <- ltfm_mkBody(object[[i,j]],ii[[j]],format[[j]],digits[[j]])
        if(n.col.vars[[j]] > 1 |  (!varontop & nzchar(nms.cv[[j]]))[1])
          tmp.bdy <- cbind("",tmp.bdy)
        if(j > 1 && !compact)
          tmp.bdy <- cbind("",tmp.bdy)
        if(varinfront){

          nm.row.vars.i <- names(row.vars[[i]])
          if(i > 1 && n.row.vars[i] > 1 && any(nzchar(nm.row.vars.i[-n.row.vars[i]])))
             tmp.bdy <- rbind("",tmp.bdy)
        }
        else{

          if(i > 1 && any(nzchar(names(row.vars[[i]]))))
            tmp.bdy <- rbind("",tmp.bdy)
        }
        body[[i,j]] <- tmp.bdy
      }
    }

    ans <- list()
    for(i in 1:N){
    
      tmp.bdy <- cbind(leaders[[i]],do.call(cbind,body[i,,drop=FALSE]))
      tmp.bdy <- apply(tmp.bdy,1,paste0,collapse="&")
      tmp.rowsep <- rep("\\\\",length(tmp.bdy))
      if(i<N && (!varinfront || max.n.row.vars > 1))
        tmp.rowsep[length(tmp.bdy)] <- paste0(tmp.rowsep[length(tmp.bdy)],"[",groupsep,"]")
      if(i>1 && (!varinfront || max.n.row.vars > 1) && any(nzchar(names(row.vars[[i]])))){
        
        tmp.rowsep[1] <- paste0(tmp.rowsep[1],"\n",grouprule)
      }
      tmp.bdy <- paste0(tmp.bdy,tmp.rowsep)
      if(i > 1 && varinfront && max.n.row.vars == 1)
        tmp.bdy[1] <- paste(grouprule,tmp.bdy[1])
      ans[[i]] <- tmp.bdy
    }
    ans <- unlist(ans)

    ans <- c(toprule,header,midrule,ans,bottomrule)

    tabspec <- mapply(ltfm_mkTabspec,colspec,ii,m,USE.NAMES=FALSE)

    for(i in 1:M){

      if(n.col.vars[i] > 1 | (!varontop & nzchar(nms.cv[[i]]))[1])
        tabspec[i] <- paste0("l",tabspec[i])
      if(i>1 & !compact)
        tabspec[i] <- paste0("c",tabspec[i])
    }
    tabspec <- c(rep("l",max.n.row.vars),tabspec)
    if(varinfront) tabspec <- c("l",tabspec)
    tabspec <- paste0(tabspec,collapse="")

    tabbegin <- paste0("\\begin{tabular}{",tabspec,"}")
    tabend <- "\\end{tabular}"
    ans <- c(tabbegin,ans,tabend)

    structure(ans,class="Latex")
}

cv_get_desc <- function(col.vars,compact){

      m <- prod(sapply(col.vars,length))
      n.col.vars <- length(col.vars)
      nms.cv <- names(col.vars)
      l.cv <- sapply(col.vars,length)
      cv.sizeg <- rev(cumprod(rev(l.cv)))
      cv.repg <- m/cv.sizeg

      if(compact){
        cv.stepg <- m/cv.repg
        width <- m
        }
      else {

        width <- cv.repg[n.col.vars]*(cv.sizeg[n.col.vars]+1)
        cv.stepg <- width/cv.repg
        width <- width - 1
      }

      mcols <- c(if(compact) cv.sizeg else cv.stepg-1,1)

      start.g <- lapply(1:n.col.vars,function(i){
                      seq(from=1,length=cv.repg[i],by=cv.stepg[i])
                    })
      end.g <- mapply(`+`,start.g,cv.stepg-2)
      start.g.n <- start.g[[n.col.vars]]
      end.g.n <- end.g[[n.col.vars]]
      ii <- unlist(mapply(seq,from=start.g.n,to=end.g.n,SIMPLIFY=FALSE))

      list(
        nms.cv=nms.cv,
        l.cv=l.cv,
        cv.sizeg=cv.sizeg,
        cv.repg=cv.repg,
        cv.stepg=cv.stepg,
        width=width,
        mcols=mcols,
        start.g=start.g,
        end.g=end.g,
        start.g.n=start.g.n,
        end.g.n=end.g.n,
        ii=ii
        )
}

ltfm_mkHeader <- function(col.vars,n.col.vars,mcols,width,cv.repg,ii,varontop,compact){

    nms.cv <- names(col.vars)

    header <- character()
    for(i in 1:n.col.vars){

      if(i==1 && varontop){
        if(nzchar(nms.cv[i]))
            tmp.header <- paste0("\\multicolumn{",mcols[i],"}{c}{",nms.cv[i],"}")
        else
            tmp.header <- ""
        if(n.col.vars>1)
          tmp.header <- paste0("&",tmp.header)
        header <- append(header,list(tmp.header))
      }

      if(i==n.col.vars){
        tmp.header <- character(width)
        tmp.header[ii] <- paste0("\\multicolumn{",mcols[i+1],"}{c}{",rep(col.vars[[i]],cv.repg[i]),"}")
        tmp.header <- paste0(tmp.header,collapse="&")
      }
      else {
        tmp.header <- paste0("\\multicolumn{",mcols[i+1],"}{c}{",rep(col.vars[[i]],cv.repg[i]),"}")
        tmp.header <- paste0(tmp.header,collapse=if(compact)"&"else"&&")
      }
      if(n.col.vars==1){

        if(nzchar(nms.cv[i]) && !varontop)
            tmp.header <- paste0(nms.cv[i],":&",tmp.header)
        
      }
      else {

        if((i > 1 || !varontop) && nzchar(nms.cv[i]))
            tmp.header <- paste0(nms.cv[i],":&",tmp.header)
        else
            tmp.header <- paste0("&",tmp.header)
      }
        
      header <- append(header,list(tmp.header))
    }
  header
}

mkclines <- function(cmidrule,start.g,end.g){

  len <- end.g - start.g + 1
  start.g <- start.g[len>1]
  end.g <- end.g[len>1]
  paste0(cmidrule,"{",start.g,"-",end.g,"}")
}

ltfm_mkLeader <- function(row.vars,n.row.vars,n) {

  leader <- matrix("",nrow=n,ncol=n.row.vars)

  for(i in 1:n.row.vars){

    rv <- row.vars[[i]]
    rep.rv <- if(i==1)1 else nn.rv
    rv <- rep(rv,rep.rv)
    nn.rv <- length(rv)
    rv.n <- n/nn.rv
    pos <- (1:nn.rv)*rv.n
    pos <- pos - rv.n + 1
    leader[pos,i] <- rv
  }
  leader
}

ltfm_mkBody <- function(object,ii,format,digits){

    n <- nrow(object)
    m <- ncol(object)

    d <- digits
    digits <- integer(m)
    digits[] <- d

    fo <- format
    format <- integer(m)
    format[] <- fo

    total.width <- max(ii)

    body <- array("",dim=c(nrow(object),total.width))

    for(i in seq(along=digits))
      body[,ii[i]] <- formatC(object[,i],digits=digits[i],format=format[i])

    sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",body)
}

ltfm_mkTabspec <- function(colspec,ii,m){

  csp <- character(m)
  csp[] <- colspec
  colspec <- csp
  
  total.width <- length(ii)
  tabspec <-rep("c",total.width)
  tabspec[ii] <- colspec
  paste(tabspec,collapse="")
}