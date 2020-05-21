#' Gaussian Error Linear Unit.
#' This is a smoother version of the RELU.
#' Original paper: https://arxiv.org/abs/1606.08415
#' Args:
#'   x: float Tensor to perform activation.
#'Returns:
#'  x with the GELU activation applied.
#' @export
gelu <- function(x) {

  cdf <- 0.5 * (1 + tf$tanh(
    (sqrt(2 / pi) * (x + 0.044715 * tf$pow(x, 3L)))))

  act <- x * cdf

  act
}


#' keras lambda layer Gaussian Error Linear Unit.
#' This is a smoother version of the RELU.
#' Original paper: https://arxiv.org/abs/1606.08415
#' Args:
#'   x: float Tensor to perform activation.
#'Returns:
#'  x with the GELU activation applied.
#' @export
layer_activation_gelu <- function(object, name = "gelu") {
  layer_lambda(object, function(x) {
    cdf = 0.5 * (1 + tf$tanh(
      (sqrt(2 / pi) * (x + 0.044715 * tf$pow(x, 3L)))))

    act <- x * cdf

    act
  }, name = name)
}


#' Bipolar ReLU as in https://arxiv.org/abs/1709.04054
#' @export
brelu <- function(x) {
  x_shape <- x$shape$dims
  c(x1, x2) %<-%
    tf$split(tf$reshape(x, c(x_shape[1:(length(x_shape) - 1)],
                             list(-1L, 2L))), 2L, axis = -1L)
  y1 <-  tf$nn$relu(x1)
  y2 <- -tf$nn$relu(-x2)

  tf$reshape(tf$concat(list(y1, y2), axis = -1L), x_shape)

}


#' Bipolar ReLU as in https://arxiv.org/abs/1709.04054
#' @export
layer_activation_brelu <- function(object, name = "brelu") {
  layer_lambda(object, function(x) {
    x_shape <- x$shape$dims
    c(x1, x2) %<-%
      tf$split(tf$reshape(x, c(x_shape[1:(length(x_shape) - 1)],
                               list(-1L, 2L))), 2L, axis = -1L)
    y1 <-  tf$nn$relu(x1)
    y2 <- -tf$nn$relu(-x2)

    tf$reshape(tf$concat(list(y1, y2), axis = -1L), x_shape)
  }, name = name)
}


#' Bipolar ELU as in https://arxiv.org/abs/1709.04054.
#' @export
belu <- function(x) {
  x_shape <- x$shape$dims
  c(x1, x2) %<-%
    tf$split(tf$reshape(x, c(x_shape[1:(length(x_shape) - 1)],
                             list(-1L, 2L))), 2L, axis = -1L)
  y1 <-  tf$nn$elu(x1)
  y2 <- -tf$nn$elu(-x2)

  tf$reshape(tf$concat(list(y1, y2), axis = -1L), x_shape)
}


#' keras layer-lambda Bipolar ELU as in https://arxiv.org/abs/1709.04054.
#' @export
layer_activation_belu <- function(object, name = "belu") {
  layer_lambda(object, function(x) {
    x_shape <- x$shape$dims
    c(x1, x2) %<-%
      tf$split(tf$reshape(x, c(x_shape[1:(length(x_shape) - 1)],
                               list(-1L, 2L))), 2L, axis = -1L)
    y1 <-  tf$nn$elu(x1)
    y2 <- -tf$nn$elu(-x2)

    tf$reshape(tf$concat(list(y1, y2), axis = -1L), x_shape)
  }, name = name)
}


#' NALU as in https://arxiv.org/abs/1808.00508
#' @export
nalu <- function(x, depth, epsilon = 1e-30, name = NULL, reuse = NULL) {
  depth   <- as.integer(depth)
  x_shape <- x$shape$dims
  x_flat  <- tf$reshape(x, list(-1L, x_shape[[length(x_shape)]]))

  gw <-
    tf$Variable(tf$random$normal(
      shape = list(x_shape[[length(x_shape)]], depth)), name = "gw")

  g      <- tf$nn$sigmoid(tf$matmul(x_flat, gw))
  g      <- tf$reshape(g, c(x_shape[1:(length(x_shape)-1)], depth))
  a      <- nac(x, depth, name = "nac_lin")

  log_x  <- tf$math$log(tf$abs(x) + epsilon)
  m      <- nac(log_x, depth, name = "nac_log")

  out <- g * a + (1 - g) * tf$exp(m)
  out
}



#' keras lambda-layer NALU as in https://arxiv.org/abs/1808.00508
#' @export
layer_activation_nalu <- function(object, depth, epsilon = 1e-30, name = NULL, reuse = NULL) {
  layer_lambda(object, function(x) {
    depth   <- as.integer(depth)
    x_shape <- x$shape$dims
    x_flat  <- tf$reshape(x, list(-1L, x_shape[[length(x_shape)]]))

    gw <-
      tf$Variable(tf$random$normal(
        shape = list(x_shape[[length(x_shape)]], depth)), name = "gw")

    g <- tf$nn$sigmoid(tf$matmul(x_flat, gw))
    g <- tf$reshape(g, c(x_shape[1:(length(x_shape)-1)], depth))
    a <- nac(x, depth, name = "nac_lin")

    log_x <- tf$math$log(tf$abs(x) + epsilon)
    m     <- nac(log_x, depth, name = "nac_log")

    out <- g * a + (1 - g) * tf$exp(m)
    out
  })
}



#' NAC as in https://arxiv.org/abs/1808.00508
#' @export
nac <- function(x, depth, name = "nac") {
  x_shape <- x$shape$dims
  w <- tf$Variable(
    tf$random$normal(shape = list(x_shape[[length(x_shape)]], depth)),
    name = "w")
  m <- tf$Variable(
    tf$random$normal(shape = list(x_shape[[length(x_shape)]], depth)),
    name = "m")

  w        <- tf$tanh(w) * tf$nn$sigmoid(m)
  x_flat   <- tf$reshape(x, list(-1L, x_shape[[length(x_shape)]]))
  res_flat <- tf$matmul(x_flat, w)

  out <- tf$reshape(res_flat, c(x_shape[1L:length(x_shape)-1], depth))
  out
}



#' keras lambda layer implementation of NAC as in https://arxiv.org/abs/1808.00508
#' @export
layer_activation_nac <- function(object, depth, name = "nac") {
  layer_lambda(object, function(x) {
    x_shape <- x$shape$dims
    depth   <- as.integer(depth)

    w <- tf$Variable(
      tf$random$normal(shape = list(x_shape[[length(x_shape)]], depth)),
      name = "w")
    m <- tf$Variable(
      tf$random$normal(shape = list(x_shape[[length(x_shape)]], depth)),
      name = "m")

    w        <- tf$tanh(w) * tf$nn$sigmoid(m)
    x_flat   <- tf$reshape(x, list(-1L, x_shape[[length(x_shape)]]))
    res_flat <- tf$matmul(x_flat, w)

    out <- tf$reshape(res_flat, c(x_shape[1L:length(x_shape)-1], depth))
    out
  })
}
