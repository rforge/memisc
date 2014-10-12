combineIndices <- function(...){

  args <- list(...)
  nargs <- length(args)
  if(nargs==1) return(args[[1]])
  combineIndicesList(x)
}

combineIndicesList <- function(x){
  nx <- length(x)
  if(nx==1) return(x[[1]])
  lx <- sapply(x,length)
  if(length(unique(lx))>1) stop("arguments have different length",call.=FALSE)
  i <- x[[1]]
  for(ii in 2:nx){
    
    #print(i)
    I <- max(i)
    j <- x[[ii]]
    J <- max(j)
    i <- i + I*(j-1)
  }
  i
}

interactIndices <- function(...){
  i <- combineIndices(...)
  match(i,sort(unique(i)))
}

rowsum2 <- function(x,...) UseMethod("rowsum2")

rowsum2.default <- function(x,...,default=NA){

  indices <- list(...)
  indices <- lapply(indices,as.numeric)
  
  idims <- sapply(indices,max,na.rm=TRUE)
  i <- combineIndicesList(indices)
  j <- match(i,unique(i))
  tmp <- rowsum(x,j)
  res <- array(default,idims)
  res[i] <- tmp[j]
  res
}

rowsum2.matrix <- function(x,...,default=NA){

  indices <- list(...)
  indices <- lapply(indices,as.numeric)
  
  idims <- sapply(indices,max)
  i <- combineIndices(indices)
  j <- match(i,unique(i))
  tmp <- rowsum(x,j)
  res <- matrix(default,nrow=prod(idims),ncol=ncol(x))
  res[i,] <- tmp[j,]
  dim(res) <- c(idims,ncol(x))
  res
}

rowsum.array <- function(x,group,reorder=TRUE,...){

   dim.x <- dim(x)
   dn.x <- dimnames(x)
   dim(x) <- c(dim.x[1],prod(dim.x[-1]))
   
   y <- rowsum.default(x,group,reorder=reorder,...)
   
   rn.y <- rownames(y)
      
   dim.y <- c(nrow(y),dim.x[-1])
   
   dn.y <- if(length(rn.y) && length(dn.x))
               c(list(rn.y),dn.x[-1])
            else if(length(dn.x))
               c(list(NULL),dn.x[-1])
            else if(length(rn.y))
               c(list(rn.y),rep(list(NULL),length(dim.x)-1))
            else NULL
            
   structure(y,
             dim=dim.y,
             dimnames=dn.y)
} 
