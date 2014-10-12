mtable_env <- environment()


.onLoad <- function(lib,pkg){

  options(coef.style="default")
  options(baselevel.sep="/")
  options(factor.style="($f): ($l)")
  options(float.style="f")
  options(signif.symbols=c(
        "***"=.001,
        "**"=.01,
        "*"=.05
    ))
}




