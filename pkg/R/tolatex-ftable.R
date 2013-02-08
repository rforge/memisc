toLatex.ftable <- function(object,
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
          extrarowsep = NULL,
          oldstyle=FALSE,
          ...)
{
    row.vars <- attr(object,"row.vars")
    col.vars <- attr(object,"col.vars")
    if(oldstyle) toLatex_oldstyle_ftable(object,
          show.titles,
          digits,
          format,
          useDcolumn,
          colspec,
          LaTeXdec,
          ddigits,
          useBooktabs,
          toprule,
          midrule,
          cmidrule,
          bottomrule,
          extrarowsep,
          ...
          )
    else {
      n <- nrow(object)
      m <- ncol(object)
      n.row.vars <- length(row.vars)
      n.col.vars <- length(col.vars)
      d <- digits
      digits <- integer(m)
      digits[] <- d
      #print(digits)
      fo <- format
      format <- integer(m)
      format[] <- fo
      body <- array("",dim=dim(object))
      for(i in seq(along=digits)) {
        #print(digits[i])
        body[,i] <- formatC(object[,i],digits=digits[i],format=format[i])
        }
      body <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",body)
      header <- vector(mode="list",2*n.col.vars)
      header.len <- length(header)
      for(i in 1:n.col.vars){

        cv <- col.vars[[i]]
        nm.cv <- names(col.vars)[i]
        rep.cv <- if(i==1)1 else nn.cv
        cv <- rep(cv,rep.cv)
        nm.cv <- rep(nm.cv,rep.cv)
        nn.cv <- length(cv)
        nn.nm.cv <- length(nm.cv)
        ncv.m <- m/nn.nm.cv
        cv.m <- m/nn.cv
        header[[2*i-1]] <- paste("\\multicolumn{",ncv.m,"}{c}{",nm.cv,"}",sep="")
        header[[2*i]] <- paste("\\multicolumn{",cv.m,"}{c}{",cv,"}",sep="")
      }

      leader <- matrix("",nrow=n,ncol=n.row.vars)
      for(i in 1:n.row.vars){

        rv <- row.vars[[i]]
        rep.rv <- if(i==1)1 else nn.rv
        rv <- rep(rv,rep.rv)
        nn.rv <- length(rv)
        rv.m <- n/nn.rv
        pos <- (1:nn.rv)*rv.m
        pos <- pos - rv.m + 1
        leader[pos,i] <- rv
      }
      lheader <- matrix("",nrow=header.len,ncol=n.row.vars)
      lheader[header.len,] <- names(row.vars)
      lheader <- apply(lheader,1,paste,collapse="&")
      header <- sapply(header,paste,collapse="&")
      header <- paste(lheader,header,sep="&")
      header <- paste(header,"\\\\",sep="")

      c.starts <- n.row.vars + 1
      c.ends <- n.row.vars + m
      cln <- paste(cmidrule,"{",c.starts,"-",c.ends,"}",sep="")
      if(n.col.vars==1)
        header[1] <- paste(header[1],"\n",cln)
      else if(n.col.vars > 1){
        hl2 <- header.len%/%2-1
        ii <- 2*(1:hl2)
        header[ii] <- paste(header[ii],"\n",cln)
        }
      
      lcv <- length(col.vars[[n.col.vars]])
      body <- apply(cbind(leader,body),1,paste,collapse="&")
      if(!length(extrarowsep))
        body <- paste(body,"\\\\",sep="")
      else {
        rowsep <- rep("\\\\",NROW(body))
        .extrarowsep <- rep("",NROW(body))
        lrv <- length(row.vars[[n.row.vars]])
        ii <- seq(NROW(body)%/%lrv)*lrv
        if(show.titles && length(names(row.vars))) ii <- ii+1
        .extrarowsep[ii] <- paste("[",extrarowsep,"]",sep="")
        rowsep <- paste(rowsep,.extrarowsep,sep="")
        body <- paste(body,rowsep,sep="")
      }
      ans <- c(toprule,header,midrule,body,bottomrule)
      leader.spec <- paste(rep("l",n.row.vars),collapse="")
      body.spec <- character(m)
      body.spec[] <- colspec
      body.spec <- paste(body.spec,collapse="")
      tabspec <- paste(leader.spec,body.spec,sep="")
      tabbegin <- paste("\\begin{tabular}{",tabspec,"}",sep="")
      tabend <- "\\end{tabular}"
      ans <- c(tabbegin,ans,tabend)
#       browser()
      structure(ans,class="Latex")
    }
}

toLatex_oldstyle_ftable <- function(object,
          show.titles=TRUE,
          digits=0,
          format="f",
          useDcolumn=TRUE,
          colspec=if(useDcolumn) paste("D{.}{",LaTeXdec,"}{",ddigits,"}",sep="") else "r",
          LaTeXdec=".",
          ddigits=digits,
          useBooktabs=TRUE,
          toprule=if(useBooktabs) "\\toprule" else "\\hline\\hline",
          midrule=if(useBooktabs) "\\midrule" else "\\hline\n",
          cmidrule=if(useBooktabs) "\\cmidrule" else "\\cline",
          bottomrule=if(useBooktabs) "\\bottomrule" else "\\hline\\hline",
          extrarowsep = NULL,
          ...){
  row.vars <- attr(object,"row.vars")
  col.vars <- attr(object,"col.vars")
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n <- nrow(object)
  m <- ncol(object)
  d <- digits
  digits <- integer(m)
  digits[] <- d
  #print(digits)
  fo <- format
  format <- integer(m)
  format[] <- fo
  body <- array("",dim=dim(object))
  for(i in seq(along=digits)) {
    #print(digits[i])
    body[,i] <- formatC(object[,i],digits=digits[i],format=format[i])
    }
  body <- sub("([eE])([-+]?[0-9]+)","\\\\textrm{\\1}\\2",body)
  #str(digits)
  #str(body)
  header <- character(n.col.vars)
  clines <- character(n.col.vars)
  ncols <- ncol(body)
  n.col.grps <- length(col.vars[[1]])
  header <- character(n.col.vars)
  clns <- character(n.col.vars)
  c.starts <- list()
  c.ends <- list()
  mcols <- integer(n.col.vars)
  ng <- integer(n.col.vars)
  mcols <- integer(n.col.vars)
  for(i in n.col.vars:1){
    if(i == n.col.vars){
      mcols[i] <- 1
      ng[i] <- ncols
    }
    else {
      mcols[i] <- length(col.vars[[i+1]])*mcols[i+1]
      ng[i] <- ncols/mcols[i]
    }
  }
  for(i in 1:n.col.vars){
    cv <- col.vars[[i]]
    lcv <- length(cv)
    tmp.header <- character(ng[i])
    tmp.header[] <- cv
    tmp.header <- paste("\\multicolumn{",mcols[i],"}{c}{",tmp.header,"}",sep="")
    if(i == n.col.vars && n.col.vars > 1){
      dim(tmp.header) <- c(mcols[i-1],ng[i-1])
      tmp.header <- apply(tmp.header,2,paste,collapse="&")
      header[i] <- paste(tmp.header,collapse="&&")
      }
    else if (n.col.vars > 1)
      header[i] <- paste(tmp.header,collapse="&&")
    else
      header[i] <- paste(tmp.header,collapse="&")
    if(length(cmidrule)){
      if(i == n.col.vars)
        c.starts[[i]] <- length(row.vars) + 2 + (seq(lcv) - 1)*mcols[i]
      else
        c.starts[[i]] <- length(row.vars) + 2 + (seq(lcv) - 1)*(mcols[i]+1)
      c.ends[[i]] <- c.starts[[i]] + (mcols[i]-1)
      tmp.cln <- paste(cmidrule,"{",c.starts[[i]],"-",c.ends[[i]],"}",sep="")
      clns[i] <- paste(tmp.cln,collapse="")
    }
  }
  for(i in 1:n.col.vars){
    if(i == n.col.vars)
      header[i] <- paste(header[i],collapse=" & ")
    else
      header[i] <- paste(header[i],collapse=" && ")
  }
  if(show.titles && length(names(row.vars))){
    hleaders <- paste("\\multicolumn{",n.row.vars,"}{l}{",names(col.vars),"}",sep="")
    hleaders <- format(hleaders)
  } else {
    hleaders <- matrix("",nrow=n.col.vars,ncol=n.row.vars)
    hleaders <- apply(hleaders,1,paste,collapse="&")
  }
  leaders <- matrix("",nrow=nrow(body),ncol=n.row.vars)
  lrv <- integer(n.row.vars)
  for(i in 1:n.row.vars){
    rv <- row.vars[[i]]
    if(i == 1)
      lrv[i] <- length(rv)
    else
      lrv[i] <- length(rv) * lrv[i-1]
    tmp.leaders <- matrix("",ncol=lrv[i],nrow=n/lrv[i])
    tmp.leaders[1,] <- rv
    leaders[,i] <- c(tmp.leaders)
  }
  if(show.titles && length(names(row.vars)))
      leaders <- rbind(names(row.vars),leaders)
  leaders <- format(leaders)
  leaders <- apply(leaders,1,paste,collapse="&")
  lcv <- length(col.vars[[n.col.vars]])
  dim(body) <- c(n,lcv,m/lcv)
  body <- format(body)
  body <- apply(body,c(1,3),paste,collapse=" & ")
  body <- apply(body,1,paste,collapse=" && ")
  if(show.titles && length(names(row.vars)))
      body <- c("",body)
  #ans <- paste(c(hleaders,leaders),c(header,body),sep=" && ")
  header <- paste(hleaders,header,sep=" && ")
  header <- paste(header,"\\\\",sep="")
  if(length(cmidrule)){
    header <- c(rbind(header,clns))
    header <- header[-length(header)]
  }
  body <- paste(leaders,body,sep=" && ")
  if(!length(extrarowsep))
    body <- paste(body,"\\\\",sep="")
  else {
    rowsep <- rep("\\\\",NROW(body))
    .extrarowsep <- rep("",NROW(body))
    lrv <- length(row.vars[[n.row.vars]])
    ii <- seq(NROW(body)%/%lrv)*lrv
    if(show.titles && length(names(row.vars))) ii <- ii+1
    .extrarowsep[ii] <- paste("[",extrarowsep,"]",sep="")
    rowsep <- paste(rowsep,.extrarowsep,sep="")
    body <- paste(body,rowsep,sep="")
  }
  if(length(cmidrule) && show.titles && length(names(row.vars))){
    rcln <- paste(cmidrule,"{",1,"-",n.row.vars,"}",sep="")
    body <- c(body[1],rcln,body[-1])
    }
  ans <- c(toprule,header,midrule,body,bottomrule)
  leader.spec <- paste(rep("l",n.row.vars+1),collapse="")
  body.spec <- character(m)
  body.spec[] <- colspec
  i <- rep(seq_len(m/lcv),each=lcv)
  body.spec <- split(body.spec,i)
  body.spec <- sapply(body.spec,paste,collapse="")
  body.spec <- paste(body.spec,collapse="c")
  tabspec <- paste(leader.spec,body.spec,sep="")
  tabbegin <- paste("\\begin{tabular}{",tabspec,"}",sep="")
  tabend <- "\\end{tabular}"
  ans <- c(tabbegin,ans,tabend)
  structure(ans,class="Latex")
}