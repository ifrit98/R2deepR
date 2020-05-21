

AntirectifierLayerNd <- R6::R6Class("KerasLayer",

  inherit = KerasLayer,

  public = list(

    axis = NULL,

    initialize = function(axis) {
      self$axis <- axis
    },

    call = function(x, mask = NULL) {
      x   <- x - k_mean(x, axis = self$axis, keepdims = TRUE)
      x   <- k_l2_normalize(x, axis = self$axis)
      pos <- k_relu(x)
      neg <- k_relu(-x)
      k_concatenate(c(pos, neg), axis = self$axis)

    },

    compute_output_shape = function(input_shape) {
      dim <-
        if (identical(self$axis, -1L)) {
          length(input_shape)
        } else
          self$axis

      input_shape[[dim]] <- input_shape[[dim]] * 2L
      input_shape
    }
  )
)



AntirectifierLayer <- R6::R6Class("KerasLayer",

  inherit = KerasLayer,

  public = list(

    call = function(x, mask = NULL) {
      x   <- x - k_mean(x, axis = 2, keepdims = TRUE)
      x   <- k_l2_normalize(x, axis = 2)
      pos <- k_relu(x)
      neg <- k_relu(-x)
      k_concatenate(c(pos, neg), axis = 2)

    },

    compute_output_shape = function(input_shape) {
      input_shape[[2]] <- input_shape[[2]] * 2L
      input_shape
    }
  )
)


#' Antirectifier layer wrapper for keras (thanks fchollet).
#'
#' Expects input shape of (examples, samples), normalizing over the `samples` dimension.
#' @export
layer_antirectifier <- function(object) {
  create_layer(AntirectifierLayer, object)
}



#' Antirectifier layer wrapper for keras (thanks fchollet) in arbitrary dimensions.
#' @param axis integer. The `samples` dimension to normalize over, defaults to 3 for 2d input.
#'   NOTE: Axis is `R` style 1-based indexing, e.g. axis = 2 refers to the 2nd dim, not 3rd.
#' @export
layer_antirectifier_nd <- function(object, axis = 3L) {
  create_layer(AntirectifierLayerNd, object, args = list(axis = as.integer(axis)))
}
