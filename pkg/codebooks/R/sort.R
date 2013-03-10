sort.data.set <- function(x,decreasing=FALSE,by=NULL,na.last=NA,...){
  if(!length(by))
    ii <- do.call(order,c(x,list(decreasing=decreasing,na.last=na.last)))
  else if(is.character(by))
    ii <- do.call(order,c(x[by],list(decreasing=decreasing,na.last=na.last)))
  else if(inherits(by,"formula")){
    x.frm <- structure(x@.Data,names=names(x),row.names=x@row_names,class="data.frame")
    bydata <- model.frame(formula=by,data=x.frm,na.action=na.pass)
    bydata <- bydata[rev(all.vars(by))]
    ii <- do.call(order,c(bydata,list(decreasing=decreasing,na.last=na.last)))
  }
  x[ii,]
}
