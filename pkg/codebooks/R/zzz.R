car_recode <- function (var, recodes, as.factor.result, levels)
  stop("package 'car' is not available")

car_pkg <-"car"
codebooks_env <- environment()


.onLoad <- function(lib,pkg){

  options(codebook.chunk.size=250000)
  options(print.use.value.labels=TRUE)
  options(show.max.obs=25)

  if(any(car_pkg == .packages(TRUE))){
    do.call("require",list(package=car_pkg))
    car_recode <- utils::getFromNamespace("recode",ns=car_pkg)
    assign("car_recode",car_recode,envir=codebooks_env)
  }

  if(any(grepl("tools:rstudio",search())) && exists("viewData")){
    setGeneric("viewData",function(x,title)standardGeneric("viewData"))
    setMethod("viewData","data.set",function(x,title){
      if (missing(title))
          title <- paste("Data set:", deparse(substitute(x))[1])

      nrow.x <- nrow(x)
      frame <- structure(x@.Data,row.names=x@row_names,names=x@names,class="data.frame")

      frame <- lapply(frame,format)
      callGeneric(frame,title=title)
    })
  }
}



.onUnload <- function(libpath)
{
    library.dynam.unload("codebooks", libpath)
}

