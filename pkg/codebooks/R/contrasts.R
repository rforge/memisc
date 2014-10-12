contr.treatment <- function (n, base = 1, contrasts = TRUE, sparse = FALSE)
{
  cont <- stats::contr.treatment(n,contrasts=FALSE,sparse=sparse)
  if(contrasts){
      nlev <- nrow(cont)
      if(!length(base)) base <- nlev
      if(is.character(base)){
          if(base %in% colnames(cont))
            base <- match(base,colnames(cont))
          else stop("undefined baseline category")
      }
      if(base < 1L | base > nlev)
        stop("baseline category number out of range")
      cont <- cont[, -base, drop=FALSE]
  }
  cont
}


contr.sum <- function (n, base = NULL, contrasts = TRUE, sparse = FALSE)
{
  cont <- stats::contr.sum(n,contrasts=FALSE,sparse=sparse)
  if(contrasts){
      nlev <- nrow(cont)
      if(!length(base)) base <- nlev
      if(is.character(base)){
          if(base %in% colnames(cont))
            base <- match(base,colnames(cont))
          else stop("undefined baseline category")
      }
      if(base < 1L | base > nlev)
        stop("baseline category number out of range")
      cont <- cont[, -base, drop=FALSE]
      cont[base,] <- -1
  }
  cont
}


dummy_labels <- function(x){
  ux <- sort(unique(as.character(x)))
  new("value.labels",ux,values=ux)
}


# copied from stats:contrasts and modified
setMethod("contrasts","item",function(x,contrasts=TRUE,sparse=FALSE)
{
    if(measurement(x) %nin% c("nominal","ordinal"))
      warning("contrasts(x,...) called with non-categorical x")
    if(!length(vl <- labels(x))) vl <- dummy_labels(x) # stop("cannot obtain contrasts for unlabelled item")
    vl <- vl[is.valid2(vl@values,x@value.filter)]
    nvl <- length(vl@values)
    labs <- vl@.Data
    if (!contrasts)
        return(structure(diag(nvl), dimnames = list(labs,
                                                    labs)))
    ctr <- attr(x, "contrasts")
    if (is.null(ctr)) {
        ctrname <- getOption("contrasts")[[if (is.nominal(x)) 1 else 2]]
        ctr <- get(ctrname, mode = "function", envir = parent.frame())(labs,
                            contrasts = contrasts)
    }
    else if (is.character(ctr)){
        ctr <- get(ctr, mode = "function", envir = parent.frame())(labs,
                            contrasts = contrasts)
    }
    else if (is.function(ctr)){
        ctr <- ctr(labs,contrasts = contrasts)
    }
    else if (is.matrix(ctr) && nrow(ctr) != nvl){
        warning("contrast matrix has wrong rows, deleting it")
        ctr <- NULL
    }
    ctr
})

# copied from stats:contrasts<- and modified
setMethod("contrasts<-","item",function(x,how.many,value){
    if(measurement(x) %nin% c("nominal","ordinal"))
      warning("contrasts(x,...) called with non-categorical x")
    if(!length(vl <- labels(x))) vl <- dummy_labels(x) #stop("cannot obtain contrasts for unlabelled item")
    vl <- vl[is.valid2(vl@values,x@value.filter)]
    nvl <- length(vl@values)
    labs <- vl@.Data

    if (nvl < 2)
        stop("contrasts can be applied only to factors with 2 or more levels")
    if (is.numeric(value)) {
        value <- as.matrix(value)
        if (nrow(value) != nvl)
            stop("wrong number of contrast matrix rows")
        n1 <- if (missing(how.many)) nvl - 1
              else how.many
        nc <- ncol(value)
        rownames(value) <- labs
        if (nc < n1) {
            cm <- qr(cbind(1, value))
            if (cm$rank != nc + 1)
                stop("singular contrast matrix")
            cm <- qr.qy(cm, diag(nvl))[, 2:nvl]
            cm[, 1:nc] <- value
            dimnames(cm) <- list(levels(x), NULL)
            if (!is.null(nmcol <- dimnames(value)[[2]]))
                dimnames(cm)[[2]] <- c(nmcol,
                              rep.int("", n1 - nc))
        }
        else cm <- value[, 1:n1, drop = FALSE]
    }
    else if (is.function(value))
        cm <- value
    else if (is.character(value))
        cm <- value
    else if (is.null(value))
        cm <- NULL
    else stop("numeric contrasts or contrast name expected")
    attr(x, "contrasts") <- cm
    x
})


contr <- function(type,...){
  call <- match.call()
  contr.fun <- as.name(paste("contr",type,sep="."))
  args <- list(n=quote(n),...,contrasts=quote(contrasts))
  fun <- function(n,contrasts=TRUE) NULL
  body(fun) <- as.call(c(contr.fun,args))
  fun
}


setMethod("as.matrix","item.vector",function(x,...){
    f <- as.factor(x)
    as.matrix(f)
})

setMethod("as.matrix","factor",function(x,...){
    i <- as.integer(x)
    as.matrix(contrasts(x))[i,,drop=FALSE]
})