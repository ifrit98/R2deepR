
#' @export
pyzip <- function(x, y) {
  purrr::map2(x, y, ~c(..1, ..2))
}

# TODO: add name support for x argument
#' @export
pyunzip <- function(x, collapse = FALSE) {
  xs <- purrr::map_depth(l, 1, ~..1[[1]])
  ys <- purrr::map_depth(l, 1, ~..1[[2]])

  if (collapse)
    return(c(xs, ys))

  list(xs = xs, ys = ys)
}


#' Named unzip.
#'
#' Separate names and values into distinct lists
#' @export
nunzip <- function(x) {
  nms  <- names(x)
  vals <- unname(unlist(x))
  list(names = nms, values = vals)
}


#' @export
rotate <- function(x, n) {
  stopifnot(n < length(x))
  if (n == 0) return(x)

  c(x[(n+1):length(x)], x[1:n])
}



#' @export
first <- function (x, n = 1) head(x, n = n)

#' @export
last <- function (x, n = 1) tail(x, n = n)


#' Convencience to build a keras model with given specification.
#'
#' @param input tensor (or keras layer_input) defining model input.
#' @param output tensor (or keras layer_input) defining model output.
#' @param optimizer keras optimizer (character or callable) to use.
#' @param loss loss function (character or callable).
#' @param metric metric to score model with.
build_and_compile <-
  function(input,
           output,
           optimizer = 'adam',
           loss = "mse",
           metric = 'acc') {
    model <- keras::keras_model(input, output) %>%
      keras::compile(optimizer = optimizer,
                     loss = loss,
                     metric = metric)
    model
  }



#' Grab list of tensor dims statically, where possible.
#' @export
shape_list <-
  function(x) {

    x <- tf$convert_to_tensor(x)

    dims <- x$get_shape()$dims
    if (is.null(dims)) return(tf$shape(x))

    sess <- tf$keras$backend$get_session()

    shape <- tf$shape(x)$eval(session = sess)

    ret <- vector('list', length(dims))

    map2(dims, shape, function(x, y) {
      dim <- x

      if (is.null(dim))
        dim <- y

      dim

    })
  }

#' Grabs list of tensor dims statically by calling .value
#' @export
shape_list2 <-
  function(x) {

    x <- tf$convert_to_tensor(x)

    dims <- x$get_shape()$dims
    if (is.null(dims)) return(tf$shape(x))

    dims <- map(dims, ~.$value)

    sess <- tf$keras$backend$get_session()
    shape <- tf$shape(x)$eval(session = sess)

    ret <- vector('list', length(dims))

    map2(dims, shape, function(x, y) {
      dim <- x

      if (is.null(dim))
        dim <- y

      dim

    })
  }


#' Formatted time
#' @export
time <-
  function() format(Sys.time(), "%Y-%m-%d_%H:%M:%S")


#' @export
is_tensor <- function(x) inherits(x, "tensorflow.tensor")


#' @export
are_tensors <- function(x) vapply(list(...), is_tensor, TRUE)


#' @importFrom magrittr %<>%
#' @export
as_tensor <- function(x, dtype = NULL, coerce_matrix = FALSE) {
  if (is.null(x))
    return(x)

  if (coerce_matrix)
    x %<>% as.matrix()

  if (is_tensor(x) && !is.null(dtype))
    tf$cast(x, dtype = dtype)
  else {
    if (is_integerish(x) && isTRUE(dtype$is_integer))
      storage.mode(x) <- "integer"
    tf$convert_to_tensor(x, dtype = dtype)
  }
}


#' Is x explicitly an object of type list
#'
#' @param x input object
#' @export
#' @examples
#' is_list(c(1,2,3))
#' is_list(list(99))
is_list <- function(x) is.vector(x) && !is.atomic(x)


#' Is x explicitly a vector with length(vector) > 1
#' @export
is_vec  <- function(x) is.vector(x) & length(x) != 1L


#' Is x an (array, list, vector) with length(x) > 1
#' @export
is_vec2 <- function(x) (is_list(x) & length(x) > 1L) | is_vec(x)


#' Are we makring the end or absence of something?
#' @export
is_sentinel <- function(x)
  c(is_empty(x), is.na(x), is.nan(x), is.null(x), is.infinite(x)) %>% any()




#' @export
plot_model <- function(model,
                       to_file = "model.png",
                       show_shapes = FALSE,
                       show_layer_names = TRUE,
                       type = c("tall", "wide")) {
  rankdir <- switch(match.arg(type),
                    tall = "TB",
                    wide = "FB")

  keras:::keras$utils$plot_model(
    model,
    to_file = to_file,
    show_shapes = show_shapes,
    show_layer_names = show_layer_names,
    rankdir = rankdir
  )
}



#' @export
shuffle <- function(x) x[sample.int(length(x))]


#' @export
shuffle_rows <- function(x) x[sample.int(length(x[,1])),]


#' @export
shuffle_cols <- function(x) x[,sample.int(length(x[1,]))]

#' @export
is_scalar <- function(x) identical(length(x), 1L)

#' @export
is_integerish <- function(x, na.ok = FALSE)
  is.numeric(x) && all(x == suppressWarnings(as.integer(x)), na.rm = na.ok)


