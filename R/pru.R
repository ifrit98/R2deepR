

PyramidalRecurrentBlock <-
  R6::R6Class(
    "PyramidalRecurrentBlock",

    inherit = KerasLayer,

    public = list(
      units = NULL,
      num_layers = NULL,
      cell_type = NULL,
      activation = NULL,
      projection_activation = NULL,
      projection_batchnorm = NULL,
      kernel_initializer = NULL,
      kernel_regularizer = NULL,
      recurrent_activation = NULL,
      recurrent_initializer = NULL,
      recurrent_regularizer = NULL,
      bias_initializer = NULL,
      bias_regularizer = NULL,
      recurrent_dropout= NULL,
      cell = NULL,
      layers = NULL,

      initialize = function(
        units,
        num_layers,
        cell_type,
        activation,
        projection_activation,
        projection_batchnorm,
        kernel_initializer,
        kernel_regularizer,
        recurrent_activation,
        recurrent_initializer,
        recurrent_regularizer,
        bias_initializer,
        bias_regularizer,
        recurrent_dropout) {

        self$units <-  units
        self$num_layers <- num_layers
        self$cell_type <- cell_type
        self$activation <- activation
        self$projection_activation <-  projection_activation
        self$projection_batchnorm <-  projection_batchnorm
        self$kernel_initializer <- kernel_initializer
        self$kernel_regularizer <- kernel_regularizer
        self$recurrent_activation <- recurrent_activation
        self$recurrent_initializer <- recurrent_initializer
        self$recurrent_regularizer <- recurrent_regularizer
        self$bias_initializer <- bias_initializer
        self$bias_regularizer <- bias_regularizer
        self$recurrent_dropout <- recurrent_dropout
      },

      build = function(input_shape) {

        stopifnot(self$cell_type %in% c("LSTM", "GRU", "RNN"))

        self$cell <- switch(
          self$cell_type,
          "LSTM" = keras::layer_lstm,
          "GRU"  = keras::later_gru,
          "RNN"  = keras::layer_simple_rnn
        )

        self$layers <- purrr::map(
          .x = vector(mode = "list", length = self$num_layers),
          .f = function(x) keras::bidirectional(layer = self$cell(
            units = self$units,
            recurrent_dropout = self$recurrent_dropout,
            return_sequences = TRUE,
            return_state = TRUE,
          ))
        )
      },


      call = function(x, mask = NULL) {

        output <- x

        i <- 0L
        for (layer in self$layers) {
          c(output,
            hidden_forward,
            context_forward,
            hidden_backward,
            context_backward) %<-% layer(output)

          output <- tf$concat(output, -1L)
          state  <- tf$concat(
            list(context_forward, context_backward), -1L)


          if (i > 0L)
            output <- output %>%
            self$pad_sequence() %>%
            layer_dense(self$units)

          if (self$projection_batchnorm)
            output %<>% layer_batch_normalization()

          if (self$projection_activation)
            output %<>% layer_activation_relu()

          i <- i + 1L
        }

        c(output, state)
      },

      pad_sequence = function(output) {

        batch           <- tf$shape(output)[1]
        sequence_length <- output$get_shape()[1]
        units           <- output$get_shape()[2]

        floormod <- tf$math$floormod(sequence_length, 2L)
        padding  <- list(c(0L, 0L),
                         c(0L, floormod),
                         c(0L, 0L))

        output <- tf$pad(output, padding)

        new_units <- tf$math$multiply(units, 2L)

        new_sequence_length <-
          tf$math$floordiv(sequence_length, 2L) + floormod

        new_shape <-
          tf$stack(list(batch, new_sequence_length, new_units))

        concat_output <- tf$reshape(output, new_shape)

        concat_output
      },


      compute_output_shape = function(input_shape) {
        output_shape <-
          list(input_shape[[1L]],
               as.integer(input_shape[[2]] %/% 2L ^ (self$num_layers - 1L)),
               input_shape[[3L]])

        output_shape
      }

    )
  )


layer_pyramidal_recurrent_block <-
  function(object,
           units,
           num_layers = 3,
           cell_type = 'LSTM',
           activation = 'tanh',
           projection_activation = TRUE,
           projection_batchnorm = TRUE,
           kernel_initializer = 'glorot_normal',
           kernel_regularizer = NULL,
           recurrent_activation = 'sigmoid',
           recurrent_initializer = 'orthogonal',
           recurrent_regularizer = NULL,
           bias_initializer = 'zeros',
           bias_regularizer = NULL,
           recurrent_dropout = 0.0,
           name = NULL,
           trainable = TRUE) {
    create_layer(
      PyramidalRecurrentBlock,
      object,
      list(
        units = as.integer(units),
        num_layers = as.integer(num_layers),
        cell_type = toupper(as.character(cell_type)),
        activation = tf$keras$activations$get(activation),
        projection_activation = projection_activation,
        projection_batchnorm = projection_batchnorm,
        kernel_initializer = tf$keras$initializers$get(kernel_initializer),
        kernel_regularizer = tf$keras$regularizers$get(kernel_regularizer),
        recurrent_activation = tf$keras$activations$get(recurrent_activation),
        recurrent_initializer = tf$keras$initializers$get(recurrent_initializer),
        recurrent_regularizer = tf$keras$regularizers$get(recurrent_regularizer),
        bias_initializer = tf$keras$initializers$get(bias_initializer),
        bias_regularizer = tf$keras$regularizers$get(bias_regularizer),
        recurrent_dropout = recurrent_dropout,
        name = name,
        trainable = trainable
      )
    )
  }
