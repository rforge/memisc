.mTableEnv <- new.env()

str.has <- function(text,has,not=NULL,how=c("all","any")){
    how <- match.fun(match.arg(how))

    hasit <- sapply(has,function(pat)regexpr(pat,text,fixed=TRUE) > 0)
    if(is.matrix(hasit))
        hasit <- apply(hasit,1,how)
    else
        hasit <- all(hasit)


    if(!length(not)) return(hasit)
    # else
    hasnot <- sapply(not,function(pat)regexpr(pat,text,fixed=TRUE) > 0)
    if(is.matrix(hasnot))
        hasnot <- apply(hasnot,1,how)
    else
        hasnot <- all(hasnot)

    hasit & !hasnot
}



setCoefTemplate <- function(...){
  args <- list(...)
  argnames <- names(args)
  CoefTemplates <- get("CoefTemplates", envir=.mTableEnv)
  OldCoefTemplates <- CoefTemplates
    for(coef.style in argnames){
      CoefTemplates[[coef.style]] <- args[[coef.style]]
  }
  assign("CoefTemplates",CoefTemplates, envir=.mTableEnv)
  return(invisible(OldCoefTemplates))
}

getFirstMatch <- function(x,n){
  for(n. in n){
    if(n. %in% names(x)) return(x[[n.]])
  }
  return(x[["default"]])
}

getCoefTemplate <- function(style){
  CoefTemplates <- get("CoefTemplates", envir=.mTableEnv)
  if(missing(style)) return(CoefTemplates)
  else return(CoefTemplates[[style]])
}


getSummary <- function(obj,alpha=.05,...) UseMethod("getSummary")

getSummary.lm <- function(obj,
            alpha=.05,
            ...
            ){
  smry <- summary(obj)
  coef <- smry$coef

  numdf <- unname(smry$fstatistic[2])
  dendf <- unname(smry$fstatistic[3])

  lower <- coef[,1] + coef[,2]*qt(p=alpha/2,df=dendf)
  upper <- coef[,1] + coef[,2]*qt(p=1-alpha/2,df=dendf)

  coef <- cbind(coef,lower,upper)

  colnames(coef) <- c("est","se","stat","p","lwr","upr")
  sigma <- smry$sigma
  r.squared <- smry$r.squared
  adj.r.squared <- smry$adj.r.squared
  F <- unname(smry$fstatistic[1])
  p <- pf(F,numdf,dendf,lower.tail=FALSE)
  N <- sum(smry$df[1:2])
  ll <- logLik(obj)
  deviance <- deviance(obj)
  AIC <- AIC(obj)
  BIC <- AIC(obj,k=log(N))
  sumstat <- c(
          sigma         = sigma,
          r.squared     = r.squared,
          adj.r.squared = adj.r.squared,
          F             = F,
          numdf         = numdf,
          dendf         = dendf,
          p             = p,
          logLik        = ll,
          deviance      = deviance,
          AIC           = AIC,
          BIC           = BIC,
          N             = N
          )

  #coef <- apply(coef,1,applyTemplate,template=coef.template)

  #sumstat <- drop(applyTemplate(sumstat,template=sumstat.template))
  list(coef=coef,sumstat=sumstat,contrasts=obj$contrasts,xlevels=obj$xlevels,call=obj$call)
}


getSummary.glm <- function(obj,
            alpha=.05,
            ...){

  smry <- summary(obj)
  N <- if(length(weights(obj))) sum(weights(obj))
    else sum(smry$df[1:2])

  coef <- smry$coef

  lower <- qnorm(p=alpha/2,mean=coef[,1],sd=coef[,2])
  upper <- qnorm(p=1-alpha/2,mean=coef[,1],sd=coef[,2])

  coef <- cbind(coef,lower,upper)

  colnames(coef) <- c("est","se","stat","p","lwr","upr")
  phi <- smry$dispersion
  LR <- smry$null.deviance - smry$deviance
  df <- smry$df.null - smry$df.residual

  ll <- logLik(obj)
  deviance <- deviance(obj)


  if(df > 0){
    p <- pchisq(LR,df,lower.tail=FALSE)
    L0.pwr <- exp(-smry$null.deviance/N)
    #LM.pwr <- exp(-smry$deviance/N)

    McFadden <- 1- smry$deviance/smry$null.deviance
    Cox.Snell <- 1 - exp(-LR/N)
    Nagelkerke <- Cox.Snell/(1-L0.pwr)
    }
  else {
    LR <- NA
    df <- NA
    p <- NA
    McFadden <- NA
    Cox.Snell <- NA
    Nagelkerke <- NA
    }

  AIC <- AIC(obj)
  BIC <- AIC(obj,k=log(N))
  sumstat <- c(
          phi         = phi,
          LR             = LR,
          df         = df,
          p             = p,
          logLik        = ll,
          deviance      = deviance,
          McFadden      = McFadden,
          Cox.Snell       = Cox.Snell,
          Nagelkerke    = Nagelkerke,
          AIC           = AIC,
          BIC           = BIC,
          N             = N
          )

  #coef <- apply(coef,1,applyTemplate,template=coef.template)

  #sumstat <- drop(applyTemplate(sumstat,template=sumstat.template))
  list(coef=coef,sumstat=sumstat,contrasts=obj$contrasts,xlevels=obj$xlevels,call=obj$call)
}

getSummaryTemplate <- function(x){
  SummaryTemplates <- get("SummaryTemplates", envir=.mTableEnv)
  if(missing(x)) return(SummaryTemplates)
  if(is.character(x)) cls <- x
  else cls <- class(x)
  return(getFirstMatch(SummaryTemplates,cls))
}

setSummaryTemplate <- function(...){
  args <- list(...)
  argnames <- names(args)
  OldSummaryTemplates <- SummaryTemplates <- get("SummaryTemplates", envir=.mTableEnv)
  for(cls in argnames){
      SummaryTemplates[[cls]] <- args[[cls]]
  }
  assign("SummaryTemplates",SummaryTemplates,envir=.mTableEnv)
  return(invisible(OldSummaryTemplates))
}

.SummaryTemplates <- list()
.CoefTemplates <- list()
.CoefTemplates$default <- c(est="($est:#)($p:*)",
                                          se="(($se:#))")
.CoefTemplates$stat <- c(est="($est:#)($p:*)",
                                      stat="(($stat:#))")
.CoefTemplates$all <- c(est="($est:#)($p:*)",
                                      se="(($se:#))",
                                      stat="(($stat:#))",
                                      p="(($p:#))"
                                      )
.CoefTemplates$all.nostar <- c(est="($est:#)",
                                      se="(($se:#))",
                                      stat="(($stat:#))",
                                      p="(($p:#))"
                                      )
.CoefTemplates$horizontal <- t(c(est="($est:#)($p:*)",
                                          se="(($se:#))"))
.CoefTemplates$ci <- c(est="($est:#)",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        )


.CoefTemplates$ci.se <- c(est="($est:#)",
                                        se="(($se:#))",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        )

.CoefTemplates$ci.se.horizontal<- matrix(c(est="($est:#)",
                                        se="(($se:#))",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        ),ncol=2,nrow=2,byrow=TRUE,
                                        dimnames=list(
                                          c("est","ci"),
                                          c("est","se")
                                          ))

.CoefTemplates$ci.p <- c(est="($est:#)",
                                        p="(($p:#))",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        )

.CoefTemplates$ci.p.horizontal<- matrix(c(est="($est:#)",
                                        p="(($p:#))",
                                        lwr="[($lwr:#)",
                                        upr="($upr:#)]"
                                        ),ncol=2,nrow=2,byrow=TRUE,
                                        dimnames=list(
                                          c("est","ci"),
                                          c("est","se")
                                          ))
.SummaryTemplates$lm <-
  c(
          "R-squared"     = "($r.squared:f#)",
          "adj. R-squared" = "($adj.r.squared:f#)",
          sigma         = "($sigma:#)",
          F             = "($F:f#)",
          p             = "($p:f#)",
          "Log-likelihood"  = "($logLik:f#)",
          Deviance      = "($deviance:f#)",
          AIC           = "($AIC:f#)",
          BIC           = "($BIC:f#)",
          N             = "($N:d)"
  )

.SummaryTemplates$glm <-
  c(
          "McFadden R-sq." = "($McFadden:f#)",
          "Cox-Snell R-sq." = "($Cox.Snell:f#)",
          "Nagelkerke R-sq."  = "($Nagelkerke:f#)",
          phi         = "($phi:#)",
          "Likelihood-ratio" = "($LR:f#)",
          p             = "($p:#)",
          "Log-likelihood" = "($logLik:f#)",
          Deviance      = "($deviance:f#)",
          AIC           = "($AIC:f#)",
          BIC           = "($BIC:f#)",
          N             = "($N:d)"
  )

.SummaryTemplates$default <-
  c(
          "McFadden R-sq." = "($McFadden:f#)",
          "Cox-Snell R-sq." = "($Cox.Snell:f#)",
          "Nagelkerke R-sq."  = "($Nagelkerke:f#)",
          "Likelihood-ratio" = "($LR:f#)",
          p             = "($p:#)",
          "Log-likelihood" = "($logLik:f#)",
          Deviance      = "($deviance:f#)",
          AIC           = "($AIC:f#)",
          BIC           = "($BIC:f#)",
          N             = "($N:d)"
  )


assign("SummaryTemplates",.SummaryTemplates, envir=.mTableEnv)
assign("CoefTemplates",.CoefTemplates, envir=.mTableEnv)



