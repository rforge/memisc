Foreach <- foreach <- function(...){
  args <- match.call(expand.dots=FALSE)$...
  tags <- names(args)
  parent <- parent.frame()
  vars <- args[nzchar(tags)]
  expr <- args[!nzchar(tags)]
  if(length(expr)) expr <- expr[[1]]
  else return()
  tags <- tags[nzchar(tags)]
  if(!length(expr) || !length(tags))return(invisible(NULL))
  vars <- sapply(vars,function(values){
      values <- if(is.call(values)) {
        if(as.character(values[[1]]) %in% c("c","list")) as.list(values[-1])
        else eval(values,parent)
        }
      else
        eval(values)
      valchars <- sapply(values,as.character)
      if(!all(nzchar(valchars))) stop("empty element in substitution list")
      values
      })
  if(length(tags)==1) {
    vars <- as.matrix(vars)
    colnames(vars) <- tags
  } else if(!is.matrix(vars)) stop("variables have unequal length")
  for(i in seq_len(nrow(vars))){
    subst <- vars[i,]
    res <- do.call("substitute",list(expr,subst))
    eval(res,parent.frame())
    }
}


xapply <- function(...){
  args <- match.call(expand.dots=FALSE)$...
  tags <- names(args)
  parent <- parent.frame()
  vars <- args[nzchar(tags)]
  expr <- args[!nzchar(tags)]
  if(length(expr)) expr <- expr[[1]]
  else return()
  tags <- tags[nzchar(tags)]
  if(!length(expr) || !length(tags))return(invisible(NULL))
  parent <- parent.frame()
  e <- evalq(environment(), list(), parent)
  vars <- sapply(vars,function(values){
      values <- if(is.call(values)) {
        if(as.character(values[[1]]) %in% c("c","list")) as.list(values[-1])
        else eval(values,parent)
        }
      else
        eval(values)
      valchars <- sapply(values,as.character)
      if(!all(nzchar(valchars))) stop("empty element in substitution list")
      values
      })
  if(length(tags)==1) {
    vars <- as.matrix(vars)
    colnames(vars) <- tags
  } else if(!is.matrix(vars)) stop("variables have unequal length")
  res <- lapply(seq_along(vars),function(i){
    subst <- vars[i,]
    res <- do.call("substitute",list(expr,subst))
    eval(res,e)
    })
  names(res) <- sapply(vars,as.character)
  res
}

syms <- function(...,paste=FALSE,sep=""){
  sep <- as.character(sep)
  args <- match.call(expand.dots=FALSE)$...
  parent <- parent.frame()
  args <- if(!length(args)) {
        tmp <- ls(parent.frame())
        i <- grep("^_",tmp)
        if(length(i)) tmp[-i] else tmp
      }
      else 
        lapply(args,function(arg){
          if(length(arg)>1) {
            if(is.call(arg)){
              if(as.character(arg[[1]]) %in% c("c","list")) as.character(arg[-1])
              else as.character(eval(arg,parent))
            }
            else as.character(arg)
          }
          else as.character(arg)
          })
  if(paste){
    res <- do.call("paste",c(args,list(sep=sep)))
    lapply(res,as.symbol)
  }
  else 
    lapply(args,as.symbol)
}

as.symbols <- function(x) lapply(x,as.symbol)


Pairs <- function(x,y=x){
  res <- outer(x,y,function(x,y)mapply(c,x,y,SIMPLIFY=FALSE))
  t(matrix(unlist(res[lower.tri(res)],recursive=FALSE),nrow=2)[2:1,])
}






