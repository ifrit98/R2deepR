

#' Replace values with their Z scores
#' @export
standardize <- function(x) (x - mean(x)) / sd(x)


#' Creates a distribution that will have values between `(-1, 1)` with `μ = 0`.
#' @export
mean_normalize <- function(x) {
  (x - mean(x)) / (max(x) - min(x))
}


#' Scale values between `(0,1)`
#' @export
minmax_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


#' Produces values in range `(0,1)`
#' @export
unit_vector_scale <- function(x) x / sum(x)


#' Scale values based on `sqrt(sum(x^2) / max(1, length(x) - 1L))`
#' @export
sqrt_scale <- function(v) {
  v <- v[!is.na(v)]
  sqrt(sum(v^2)/max(1, length(v) - 1L))
}


#' Perform feature scaling on a vector, matrix, data.frame, or tibble
#'
#' @param x One of (data.frame, matrix, tibble)
#' @param type string indicating the type of supported scaling function.  Supported
#'   types: c(standardization, mean scaling, minmax scaling, unit vector scaling, sqrt)
#'   default: standardize
#' @param margin margin to apply over (default: 2L)
#' @export
scaler <- function(x, type = "standard", margin = 2L, ...) {

  type <- match.arg(type, c("standard","mean", "minmax", "unit", "sqrt"))
  f <- switch(
    type,
    standard = standardize,
    mean = mean_normalize,
    minmax = minmax_scale,
    unit = unit_vector_scale,
    sqrt = sqrt_scale,
    stop("Unrecognized scaler type")
  )

  UseMethod("scaler", x)
}


scaler.matrix <- function(x, ...) {
  oc <- class(x)

  if(!"matrix" %in% class(x))
    x <- as.matrix(x)

  scale <- apply(x, 2L, f)

  body <- rlang::fn_body(f) %>% as.character()
  if (any(str_detect(body, "sqrt")))
    x <- sweep(x, 2L, scale, check.margin = FALSE)
  else
    x <- scale

  if (length(oc) > 1) oc <- oc[[1]]
  switch(oc,
         matrix = as.matrix(x),
         data.frame = as.data.frame(x),
         tbl_df = as_tibble(x))
}

#' @export
scaler.data.frame <- scaler.matrix


# scaler.default <-
function(x, f, margin = 2L, center = FALSE, type = "standard") {
  x <- as.matrix(x)
  scaler.matrix(x, f, margin = margin, center = center)
}


if (FALSE) {

  x  <- mtcars
  nc <- ncol(mtcars)

  # Center data
  center <- colMeans(x, na.rm = TRUE)
  y <- sweep(x, 2L, center, check.margin = FALSE)

  # Scale data
  f <- function(v) {
    v <- v[!is.na(v)]
    sqrt(sum(v^2)/max(1, length(v) - 1L))
  }
  scale <- apply(y, 2L, f)
  z <- sweep(y, 2L, scale, "/", check.margin = FALSE)


  scaler(mtcars)
  tbl <- as_tibble(mtcars)
  scaler(tbl)


  x1 <- rnorm(10, 400, 30) %>% as.integer()
  x2 <- sample.int(10, 10, replace = TRUE)

  standardize(x1)
  standardize(x2)

  mean_normalize(x1)
  mean_normalize(x2)

  minmax_scale(x1)
  minmax_scale(x2)

  unit_vector_scale(x1)
  unit_vector_scale(x2)

}
