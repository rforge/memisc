push.locale <- function(newlocale,LC_STRINGS=c("LC_CTYPE","LC_COLLATE")){
  oldlocale <- lapply(LC_STRINGS,Sys.getlocale)
  for(lstr in LC_STRINGS)
    Sys.setlocale(category=lstr,locale=newlocale)
  return(oldlocale)
}

restore.locale <- function(loclist){
  for(n in names(loclist))
    Sys.setlocale(category=n,locale=loclist[[n]])
}