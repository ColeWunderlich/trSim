#' @exportClass TestClass

setClass('TestClass', slots=c(a="numeric",b='matrix',c="numeric"))

#' @export
bob = function(a,b,c)
{
  new('TestClass',a=a,b=b,c=c)
}
