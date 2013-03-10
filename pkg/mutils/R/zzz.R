
car_recode <- function (var, recodes, as.factor.result, levels)
  stop("package 'car' is not available")

car_pkg <-"car"
me_utils_env <- environment()


.onLoad <- function(lib,pkg){
  options(Simulation.chunk.size=1000)
  options(print.use.value.labels=TRUE)
  options(show.max.obs=25)

  if(any(car_pkg == .packages(TRUE))){
    car_recode <- getFromNamespace("recode",ns=car_pkg)
    assign("car_recode",car_recode,envir=me_utils_env)
  }

}

.onUnload <- function(libpath)
{
}


