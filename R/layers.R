

#' @export
layer_expand_dims <- function(object, axis = -1L, ...) {
  layer_lambda(object, function(x) {
    tf$expand_dims(x, as.integer(axis))
  }, name = "expand_dims")
}


#' @export
layer_squeeze <- function(object, axis = -1L, ...) {
  layer_lambda(object, function(x) tf$squeeze(x, as.integer(axis), name = "squeeze"))
}


#' A differentiable version of tf$one_hot(tf$argmax(x, -1L), nchannels)
#' @export
layer_argmax <- function(object, ...) {
  layer_lambda(object, function(x) {
    y <- tf$sign(tf$reduce_max(x, axis = -1L, keepdims = TRUE) - x)
    y <- (y - 1) * (-1)
    y
  }, ...)
}
