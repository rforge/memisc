percent2 <- function(x,by,...) UseMethod("percent2")

percent2.default <- function(x,by,weights=NULL,total=!(se || ci),
                      se=FALSE,ci=FALSE,ci.level=.95,
                      total.name="N",perc.label="Percentage",
                      place.total=c("values","separate"),...){
  subst <- substitute(x)
  x.label <- paste(deparse(subst))
  tab <- Table2(x,by=by,weights=weights,total=FALSE,percentage=FALSE,counts=TRUE,style="table",...)
  tab <- t(tab)
  tabsum <- rowSums(tab)
  perc <- 100*tab/tabsum
  if(total.name %in% unlist(dimnames(perc))) total.name <- paste("_",total.name,"_",sep="")
 #names(perc) <- rownames(tab)
  if(!se && !ci){
    if(total){
      place.total <- match.arg(place.total)
      perc <- switch(place.total,
                      values={
                        tabsum <- as.matrix(tabsum)
                        colnames(tabsum) <- total.name
                        tmp <- cbind(perc,tabsum)
                        names(dimnames(tmp))[2] <- x.label
                        names(dimnames(tmp))[is.na(names(dimnames(tmp)))] <- ""
                        tmp
                      },
                      separate={
                          tabsum <- array(tabsum,dim(perc))
                          tmp <- array(NA,c(prod(dim(perc)),2))
                          tmp[,1] <- perc
                          tmp[,2] <- tabsum
                          dim(tmp) <- c(dim(perc),2)
                          dimnames(tmp) <- c(dimnames(perc),list(c(perc.label,total.name)))
                          names(dimnames(tmp))[2] <- x.label
                          names(dimnames(tmp))[is.na(names(dimnames(tmp)))] <- ""
                          tmp
                          }
                      )
      }
  }
  else {
      prop <- tab/tabsum
      var.prop <- prop*(1-prop)/tabsum
      se.perc <- 100*sqrt(var.prop)
    if(se){
      tmp <- array(NA,c(prod(dim(perc)),2))
      tmp[,1] <- perc
      tmp[,2] <- se.perc
      dim(tmp) <- c(dim(perc),2)
      dimnames(tmp) <- c(dimnames(perc),list(c(perc.label,"se")))
      names(dimnames(tmp))[2] <- x.label
      names(dimnames(tmp))[is.na(names(dimnames(tmp)))] <- ""
      perc <- tmp
    }
    if(ci){
      alpha <- (1-ci.level)/2
      lower <- upper <- array(NA,dim(perc)[1:2])
      isnull <- tab == 0
      isfull <- tab == tabsum
      lower[!isnull] <- qbeta(alpha,tab[!isnull],(tabsum-tab+1)[!isnull])
      lower[isnull] <- 0
      upper[!isfull] <- qbeta(1-alpha,(tab+1)[!isfull],(tabsum-tab)[!isfull])
      upper[isfull] <- 1
      if(length(dim(perc))==2){
        tmp <- array(NA,c(prod(dim(perc)),3))
        tmp[,1] <- perc
        tmp[,2] <- 100*lower
        tmp[,3] <- 100*upper
        dim(tmp) <- c(dim(perc),3)
        dimnames(tmp) <- c(dimnames(perc),list(perc.label,"lower","upper"))
        perc <- tmp
        }
      else {
        n3 <- dim(perc)[3]
        tmp <- array(NA,c(prod(dim(perc)),n3+2))
        tmp[,1:n3] <- perc
        tmp[,n3+1] <- 100*lower
        tmp[,n3+2] <- 100*upper
        dim(tmp) <- c(dim(perc),n3+2)
        dimnames(tmp) <- c(dimnames(perc)[1:2],
                          c(dimnames(perc)[[3]],"lower","upper")
                          )
        perc <- tmp
      }
    }
    if(total){
      if(length(dim(perc))==2){
        tmp <- array(NA,c(prod(dim(perc)),2))
        tmp[,1] <- perc
        tmp[,2] <- tabsum
        dim(tmp) <- c(dim(perc),2)
        dimnames(tmp) <- c(dimnames(perc),list(perc.label,total.name))
        perc <- tmp
        }
      else {
        n3 <- dim(perc)[3]
        tmp <- array(NA,c(prod(dim(perc)[1:2]),n3+1))
        tmp[,1:n3] <- perc
        tmp[,n3+1] <- tabsum
        dim(tmp) <- c(dim(perc)[1:2],n3+1)
        dimnames(tmp) <- c(dimnames(perc)[1:2],
                          list(c(dimnames(perc)[[3]],total.name))
                          )
        perc <- tmp
      }
    }
    names(dimnames(perc))[2] <- x.label
    names(dimnames(perc))[is.na(names(dimnames(perc)))] <- ""
    perc
  }
  if(length(perc)){
    dimnames(perc)[[1]] <- seq_len(dim(perc)[1])
    names(dimnames(perc))[1] <- "..by.."
    if(length(dim(perc))==2) perc <- t(perc)
    else perc <- aperm(perc,c(2,3,1))
    structure(perc,class="table")
  }
}

percent2.logical <- function(x,by,weights=NULL,total=!(se || ci),
                      se=FALSE,ci=FALSE,ci.level=.95,
                      total.name="N",perc.label="Percentage",...){
  subst <- substitute(x)
  x.label <- paste(deparse(subst))
  tab <- Table2(x,by=by,weights=weights,total=FALSE,percentage=FALSE,counts=TRUE,style="table",...)
  tab <- t(tab)
  N <- rowSums(tab)
  n <- tab[,"TRUE"]
  p <- n/N
  perc <- 100*p
  if(se){
    var <- p*(1-p)/N
    perc <- cbind(perc,100*sqrt(var))
    colnames(perc) <- c(x.label,"se")
  }
  if(ci){
    alpha <- (1-ci.level)/2
    lower <- array(0,dim(p))
    upper <- array(1,dim(p))
    lower[p>0] <- qbeta(alpha,n[p>0],(N-n+1)[p>0])
    upper[p<1] <- qbeta(1-alpha,(n+1)[p<1],(N-n)[p<1])
    lower[!is.finite(p)] <- NA
    upper[!is.finite(p)] <- NA
    if(NCOL(perc)>1) perc <- cbind(perc,lower=100*lower,upper=100*upper)
    else {
      perc <- cbind(perc,100*lower,100*upper)
      colnames(perc) <- c(x.label,"lower","upper")
    }
  }
  if(total){
    if(NCOL(perc)>1)
      cn <- c(colnames(perc),total.name)
    else
      cn <- c(x.label,total.name)
    perc <- cbind(perc,N)
    colnames(perc) <- cn
  }
  t(perc)
}

