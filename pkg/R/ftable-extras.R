# ctable <- function(...){
#     if (!length(match.call(expand.dots = FALSE)$...))
#         return(NULL)
#     else UseMethod("ctable", ..1)
# }

cbind_ftable <- function(args){

  argnames <- names(args)

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

  structure(args,
    class="cbound_ftables"
    )
}

rbind_ftable <- function(args){

  argnames <- names(args)

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

  structure(args,
    class="rbound_ftables"
    )
}

format.cbound_ftables <- function(x,
  quote = TRUE,
  digits = getOption("digits"),...){

  n <- length(x)
  nms <- names(x)

  rv <- lapply(x,attr,"row.vars")
  cv <- lapply(x,attr,"col.vars")
  lrv <- sapply(rv,length)
  lcv <- sapply(cv,length)
  max.lcv <- max(lcv)
  h.hght <- if(length(nms)) max.lcv+1 else max.lcv
  
  ftd <- lapply(x,format,quote=quote,digits=digits)
  m <- nrow(ftd[[1]])-lcv[1]
  ii <- 1:m+h.hght
  front <- matrix("",nrow=m+h.hght,ncol=lrv[1])
  
  front[ii,] <- ftd[[1]][-(1:lcv[1]),1:lrv[1],drop=FALSE]

  body <- vector("list",length=n)
  headers <- vector("list",length=n)
  
  for(i in 1:n){
    cur <- ftd[[i]]
    tmp.hdr <- matrix("",nrow=h.hght,ncol=ncol(cur)-lrv[i])
    ii <- seq(from=h.hght-lcv[i]+1,to=h.hght)
    if(length(nms))
      tmp.hdr[1,1] <- nms[i]
    tmp.hdr[ii,] <- cur[1:lcv[i],-(1:lrv[i]),drop=FALSE]
    headers[[i]] <- tmp.hdr
    body[[i]] <- cur[-(1:lcv[i]),-(1:lrv[i]),drop=FALSE]
  }
  
  header <- do.call(cbind,headers)
  body <- do.call(cbind,body)
  format(cbind(front,rbind(header,body)))
}

write.cbound_ftables <- function(x,
                            file = "",
                            quote = TRUE,
                            append = FALSE,
                            digits = getOption("digits")){
  r <- format.cbound_ftables(x,quote=quote,digits=digits)
  cat(t(r), file = file, append = append, sep = c(rep(" ",
      ncol(r) - 1), "\n"))
  invisible(x)
}

print.cbound_ftables <- function(x,digits=getOption("digits"),...)
  write.cbound_ftables(x,quote=FALSE,digits=digits)

format.rbound_ftables <- function(x,
  quote = TRUE,
  digits = getOption("digits"),...){

  n <- length(x)
  nms <- names(x)

  rv <- lapply(x,attr,"row.vars")
  cv <- lapply(x,attr,"col.vars")
  lrv <- sapply(rv,length)
  lcv <- sapply(cv,length)
  max.lcv <- max(lcv)
  max.lrv <- max(lrv)
  
  f.wdth <- if(length(nms)) max.lrv[1]+1 else max.lrv[1]

  ftd <- lapply(x,format,quote=quote,digits=digits)
  
  header <- ftd[[1]][1:lcv[1],-(1:lrv[1]),drop=FALSE]

  body <- vector("list",length=n)
  front <- vector("list",length=n)

  for(i in 1:n){
    cur <- ftd[[i]]
    tmp.frnt <- matrix("",ncol=f.wdth,nrow=nrow(cur)-lcv[i])
    #ii <- 1:lrv[i]
    ii <- seq(from=f.wdth-lrv[i]+1,to=f.wdth)
    if(length(nms)){
      tmp.frnt[1,1] <- nms[i]
      ii <- ii + 1
      }
    tmp.frnt[,ii] <- cur[-(1:lcv[i]),1:lrv[i],drop=FALSE]
    front[[i]] <- tmp.frnt
    body[[i]] <- cur[-(1:lcv[i]),-(1:lrv[i]),drop=FALSE]
  }

  front <- do.call(rbind,front)
  front <- rbind(matrix("",nrow=nrow(header),ncol=ncol(front)),front)
  body <- format(do.call(rbind,body),justify="r")

  format(cbind(front,format(rbind(header,body),justify="r")))
}

write.rbound_ftables <- function(x,
                            file = "",
                            quote = TRUE,
                            append = FALSE,
                            digits = getOption("digits")){
  r <- format.rbound_ftables(x,quote=quote,digits=digits)
  cat(t(r), file = file, append = append, sep = c(rep(" ",
      ncol(r) - 1), "\n"))
  invisible(x)
}

print.rbound_ftables <- function(x,digits=getOption("digits"),...)
  write.rbound_ftables(x,quote=FALSE,digits=digits)

  
cbind.ftable <- function(..., deparse.level=1){

  args <- list(...)
  cbind_ftable(args)
}

rbind.ftable <- function(..., deparse.level=1){

  args <- list(...)
  rbind_ftable(args)
}