anyInf <- function(data){
  any(vapply(data, function(x) any(is.infinite(x)), logical(1)))
}