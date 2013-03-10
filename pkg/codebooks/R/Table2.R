Table2.item.vector <- function(x,
            by,
            weights=NULL,
            counts=TRUE,
            percentage=(style=="codebook"),
            style=c("table","codebook","nolabels"),
            include.missings=(style=="codebook"),
            missing.marker=if(style=="codebook") "M" else "*",
            ...){
      if(!(counts || percentage)) stop("either counts or percentage must be TRUE")
      is.m <- is.missing(x)
      isNA <- is.na(x)
      unique.by <- unique(by)
      i <- match(by,unique.by)
      n.i <- length(unique.by)
      style <- match.arg(style)
      if (style %in% c("table","codebook")) {
        vl <- labels(x)
        if(length(vl)){
          vvl <- vl@values
          lvl <- vl@.Data
          valid <- !is.missing2(vvl,x@value.filter)
          j <- match(x@.Data,vvl,nomatch=0L)
          n.j <- length(vvl)
          nbins <- n.i * n.j
          ij <- i + (j-1)*n.i
          ij[j==0] <- 0
          if(!length(weights)){
            tab <- tabulate(ij,nbins=nbins)
            dim(tab) <- c(n.i,n.j)
            }
          else {
            f <- factor(ij,levels=1:nbins)
            good <- is.finite(weights) & is.finite(f)
            tmp <- rowsum(weights[good],f[good])
            tab <- structure(rep(0,nbins),names=levels(f))
            tab[rownames(tmp)] <- tmp[]
            dim(tab) <- c(n.i,n.j)
          }
          colnames(tab) <- as.character(vvl)
          lab <- if(style=="codebook") sQuote(lvl) else lvl
        }
        else {
          valid <- logical(0)
          tab <- integer(0)
          lab <- c()
          j <- logical(length(x))
        }
        if(!length(weights)){
          ovld <- rowsum(as.integer(!is.m & !j),i)
          omiss <- rowsum(as.integer(is.m & !j & !isNA),i)
          NAs <- rowsum(as.integer(isNA),i)
        }
        else {
          good <- is.finite(weights)
          weights <- weights[good]
          is.m <- is.m[good]
          j <- j[good]
          isNA <- isNA[good]
          ovld <- rowsum(weights*(!is.m & !j),i)
          omiss <- rowsum(weights*(is.m & !j & !isNA),i)
          NAs <- rowsum(weights*(isNA),i)
        }
        #browser()
        if(any(as.logical(ovld))){
          tab <- cbind(tab," "=ovld)
          if(style=="codebook")
            lab <- c(lab,"(unlab.vld.)")
          else {
            lab <- if(length(vl)) c(lab,"Other valid") else c(lab,"Valid")
          }
          valid <- c(valid,TRUE)
          }
        if(include.missings){
          if(any(as.logical(omiss))){
            tab <- cbind(tab," "=omiss)
            if(style == "codebook")
              lab <- c(lab,"(unlab.mss.)")
            else {
              if(length(vl)){
                lab <- c(lab,"Other missing")
              } else {
                lab <- c(lab,"Missing")
                missing.marker <- ""
              }
            }
            valid <- c(valid,FALSE)
            }
          if(any(as.logical(NAs))){
            tab <- cbind(tab,"NA"=NAs)
            if(style == "codebook")
              lab <- c(lab,"")
            else {
                lab <- c(lab,"NA")
              if(!length(vl)){
                missing.marker <- ""
              }
            }
            valid <- c(valid,FALSE)
            }
          if(length(missing.marker)){
            missing.marker <- missing.marker[1]
            if(style=="codebook"){
              valid.marker <- paste(rep(" ",nchar(missing.marker)),collapse="")
              lab <- paste(ifelse(valid,valid.marker,missing.marker),lab)
              colnames(tab) <- paste(format(colnames(tab),justify="right"),format(lab,justify="left"))
            }
            else {
              lab <- paste(ifelse(valid,"",missing.marker),lab,sep="")
              colnames(tab) <- lab
            }
          }
        } else {
          if(style=="codebook")
            colnames(tab) <- paste(format(colnames(tab),justify="right"),format(lab,justify="left"))
          else
            colnames(tab) <- lab
          tab <- tab[,valid,drop=FALSE]
        }
      }
      else { # style == "nolabels"
        if(include.missings){
          if(!length(weights)){
            NAs <- rowsum(isNA,i)
            tab <- table(i,x@.Data)
          }
          else {
            good <- is.finite(weights)
            weights <- weights[good]
            isNA <- isNA[good]
            NAs <- rowsum(weights*isNA,i)
            j <- x@.Data[good]
            ux <- sort(unique(j))
            j <- match(j,ux)
            n.j <- length(ux)
            nbins <- n.i * n.j
            ij <- i + (j-1)*n.i
            f <- factor(ij,1:nbins)
            tab <- rowsum(weights,f[good])
            dim(tab) <- c(n.i,n.j)
          }
          if(any(as.logical(NAs)))
            tab <- cbind(tab,"NA"=NAs)
          if(length(missing.marker)){
            missing.marker <- missing.marker[1]
            valid <- !is.missing2(sort(unique(x@.Data)),x@value.filter)
            if(NAs)
              valid <- c(valid,FALSE)
            lab <- paste(ifelse(valid,"",missing.marker),colnames(tab),sep="")
            colnames(tab) <- lab
          }
        }
        else if(!length(weights)){
            tab <- table(i[!is.m],x@.Data[!is.m])
          }
          else {
            good <- is.finite(weights) & !is.m
            j <- x@.Data[good]
            ux <- sort(unique(j))
            j <- match(j,ux)
            n.j <- length(ux)
            nbins <- n.i * n.j
            ij <- i + (j-1)*n.i
            weights <- weights[good]
            f <- factor(ij,1:nbins)
            tab <- rowsum(weights,f[good])
            dim(tab) <- c(n.i,n.j)
          }
      }
      if(include.missings){
        if(percentage && counts) {
          vperc <- array(NA,dim(tab))
          vtab <- tab[,valid,drop=FALSE]
          Nvalid <- rowSums(vtab)
          if(any(as.logical(Nvalid))) vperc[,valid] <- 100 * vtab/Nvalid
          else vperc[,valid] <- 0
          tperc <- 100 * tab/rowSums(tab)
          ttab <- tab
          tab <- array(NA,c(dim(tab),3))
          tab[,,1] <- ttab
          tab[,,2] <- vperc
          tab[,,3] <- tperc
          dimnames(tab) <- c(dimnames(ttab),
                            Statistics=list(c("Counts","Valid","Total"))
                            )
          }
        else if(percentage) {
          vperc <- array(NA,dim(tab))
          vtab <- tab[,valid,drop=FALSE]
          Nvalid <- any(as.logical(rowSums(vtab)))
          if(Nvalid) vperc[,valid] <- 100 * vtab/Nvalid
          else vperc[,valid] <- 0
          tperc <- 100 * tab/rowSums(tab)
          ttab <- tab
          tab <- array(NA,c(dim(tab),2))
          tab[,,1] <- vperc
          tab[,,2] <- tperc
          dimnames(tab) <- c(dimnames(ttab),
                            Statistics=list(c("Valid","Total"))
                            )
        }
      }
      else {
        if(percentage && counts) {
          perc <- 100 * tab/rowSums(tab)
          ttab <- tab
          tab <- array(NA,c(dim(tab),2))
          tab[,,1] <- ttab
          tab[,,2] <- perc
          dimnames(tab) <- c(dimnames(ttab),
                            Statistics=list(c("Counts","Percent"))
                            )
          }
        else if(percentage) {
          tab <- 100 * tab/rowSums(tab)
        }
      }
      

     if(length(tab)){
      dimnames(tab)[[1]] <- seq_len(dim(tab)[1])
      names(dimnames(tab))[1] <- "..by.."
      if(length(dim(tab))==2) tab <- t(tab)
      else tab <- aperm(tab,c(2,3,1))
      structure(tab,class="table")
      }
     else integer(0)
}

