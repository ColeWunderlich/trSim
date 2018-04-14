setGeneric("plotSpace",function(object) standardGeneric("plotSpace"))

#' @include class-trModel.R
#' @export
setMethod("plotSpace",signature(object="trModel"),function(object)
{
  plotData = data.frame(object@points)
  plt=ggplot(plotData,aes(x=x,y=y,color=as.factor(type)))+geom_point()+labs(color="Type")+xlim(0,object@spaceRes)+ylim(0,object@spaceRes)
  return(plt)
})
