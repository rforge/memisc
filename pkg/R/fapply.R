quickInteraction <- function(by){
  if(is.list(by)){
    n.arg <- length(by)
    f <- 0L
    uf <- 0L
    for(i in rev(1:n.arg)){
      y <- by[[i]]
      y <- as.numeric(y)
      uy <- unique(na.omit(y))
      y <- match(y,uy,NA)
      l <- length(uy)
      f <- f*l + y - 1
      uf <- unique(na.omit(f))
      f <- match(f,uf,NA)
      uf <- seq(length(uf))
    }
  }
  else {
    by <- as.numeric(by)
    uf <- unique(na.omit(by))
    f <- match(by,uf,NA)
    uf <- seq(length(uf))
  }
  return(structure(f,unique=uf))
}

fapply <- function(formula,data,...) UseMethod("fapply",data)

fapply.default <- function (formula,
                        data,
                        subset=NULL,
                        addFreq=TRUE,
                        ...)
{
    m <- match.call(expand.dots = FALSE)
    dots <- m$...
    if(attr(terms(formula,data=data),"response")){
      fcall <- formula[[2]]
      formula <- formula[-2]
      }
    else
      fcall <- NULL

    #names(m)[2] <- "formula"
    m$formula <- formula
    if (is.matrix(data))
        m$data <- data <- as.data.frame(data)
    m$... <- m$exclude <- m$drop.unused.levels <- m$names <- m$addFreq <- NULL
    #m <- m[c(1,3,2)]
    m[[1]] <- as.name("model.frame")
    if(!missing(subset)) m$subset <- NULL
    else m$subset <- eval(substitute(subset),parent.frame())
    m$data <- data
    by <- eval(m,parent.frame())
    omitted <- attr(by,"na.action")
    if(as.character(formula[[2]])[1]==".")
      by <- by[setdiff(names(by),all.vars(fcall))]

    if(length(fcall)){
      if(length(fcall)==1){
        makeTableCall <- FALSE
        fcall.c <- as.character(fcall)
        if(is.table(data)
            && fcall.c  %in% names(dimnames(data)))
            makeTableCall <- TRUE
        if(is.data.frame(data)
            && is.factor(data[[fcall.c]]))
            makeTableCall <- TRUE
        if(is.environment(data)
            && exists(fcall.c,envir=data)
            && is.factor(get(fcall.c,envir=data)))
            makeTableCall <- TRUE
        if(makeTableCall)
          fcall <- as.call(c(as.symbol("table"),fcall))
      }
      if(addFreq){
        if(length(fcall) > 1 &&
            as.character(fcall[[1]]) %in% c("table","Table","percent","nvalid") &&
            !("weights" %in% names(fcall))
          ){
          if(is.table(data) || (is.data.frame(data) && "Freq" %in% names(data))){
            fcall[[3]] <- as.symbol("Freq")
            if(as.character(fcall[[1]])=="table")
              fcall[[1]] <- as.symbol("Table")
            by <- by[setdiff(names(by),all.vars(fcall))]
            }
        }
      }
      if(length(dots)) fcall <- as.call(c(as.list(fcall),dots))
      resp.var.formula <- parse(text=paste("~",paste(all.vars(fcall),collapse="+")))[[1]]
      environment(resp.var.formula) <- environment(formula)
      m$formula <- resp.var.formula
      m$na.action <- na.pass
      data <- eval(m, parent.frame())
      if(length(omitted))
        data <- data[-omitted,,drop=FALSE]
    }

    BY <- quickInteraction(by)

    fntBY <- is.finite(BY)
    BY <- BY[fntBY]
    by <- by[fntBY,,drop=FALSE]
    data <- data[fntBY,,drop=FALSE]

    if(length(fcall)>1)
      rows <- seq_len(nrow(data))
    else
      rows <- seq_len(length(BY))

    rows <- split.default(rows,BY)


    good <- TRUE
    if(length(fcall)>1 && (as.character(fcall[[1]]) %in% c("table","Table","percent"))){
      actualcall <- fcall
      actualcall[[1]] <- switch(as.character(fcall[[1]]),
                         table=,
                         Table=Table2,
                         percent=percent2
                      )
      actualcall <- c(as.list(actualcall),list(by=BY))
      actualcall <- as.call(actualcall)
      res <- eval(actualcall,data,enclos=parent.frame())
    }
    else if(length(fcall)>1){
      data <- data[all.vars(fcall)]
      res <- lapply(rows,function(i)
                    eval(fcall,
                    data[i,,drop=FALSE],
                    enclos=parent.frame()
                ))
      good <- sapply(res,length) > 0
      if(!all(good))
        res <- res[good]
      if(as.character(fcall[[1]]) %in% c("table","Table")){
        if(length(dim(res[[1]]))<2) res <- lapply(res,c)
      }
    }
    else if(length(fcall)==1){
      data <- data[[all.vars(fcall)]]
      res <- c(rowsum(x=data,group=BY,reorder=FALSE,na.rm=FALSE))
    }
    else {
      res <- tabulate(BY,nbins=length(attr(BY,"unique")))
    }
    urows <- sapply(rows,function(ix)ix[1])
    by <- by[urows,,drop=FALSE]
    if(!all(good))
      by <- by[good,,drop=FALSE]
    ii <- do.call("order",rev(by))

    if(is.array(res)){
      iii <- lapply(dim(res),seq_len)
      iii[[length(iii)]] <- ii
      res <- do.call("[",c(list(res),iii))
    }
    else
      res <- res[ii]
    by <- by[ii,,drop=FALSE]

    structure(res,
      by=by,
      formula=formula
      )
}

