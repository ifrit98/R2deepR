
#' Modifies the keras history object during training to
#' include the learning rates of each epoch as a metric
#' @export
add_lr_to_history <- function(verbose = TRUE) callback_lambda(
  on_train_begin = function(logs = NULL) {
    metrics <- model$history$params$metrics
    model$history$params$metrics <<- c(metrics, "learning_rate")
  },
  on_epoch_end = function(epoch, logs = NULL) {
    lr <- tf$keras$backend$get_value(model$optimizer$learning_rate)
    lrs <- model$history$history$learn_rate
    if (verbose) cat("Learning Rate: ", lr, "\n")
    model$history$history$learn_rate <- c(lrs, lr)
  }
)



#' Custom callback for saving epoch optimizer weights
CallbackOptCheckpoint <- R6::R6Class(
  "KerasCallback",
  inherit = keras::KerasCallback,
  public = list(
    monitor = NULL,
    cur_min = NULL,
    training_log = NULL,
    prepend = NULL,

    initialize = function(monitor = "loss",
                          training_log = "training_log.csv",
                          prepend = NULL) {
      self$monitor = monitor
      self$cur_min = Inf
      self$training_log = training_log
      self$prepend = prepend
    },

    on_epoch_end = function(epoch, logs = list()) {
      loss <-
        read.csv(file.path(run_dir(), self$training_log))[[self$monitor]]
      cur_loss <- tail(loss, 1)

      if (cur_loss < self$cur_min) {
        save_file <- file.path(run_dir(),
                               paste0(self$prepend,
                                      'optimizer-weights-best-checkpoint.rds'))

        cat(
          paste0(
            "\nEpoch ",
            sprintf("%05d", epoch + 1),
            ": loss improved from ",
            round(self$cur_min, 5),
            " to ",
            round(cur_loss, 5),
            ", saving optimizer to ",
            save_file,
            '\n'
          )
        )

        self$cur_min <- cur_loss

        opt_weights <- self$model$optimizer$get_weights()
        saveRDS(opt_weights, file = save_file)
        # save_model_weights_hdf5(self$model, file = 'latest-model-weights.hdf5')
      } else {
        cat(paste0(
          "\nEpoch ",
          sprintf("%05d", epoch + 1),
          ": loss did not improve from ",
          round(self$cur_min, 5),
          '\n'
        ))
      }
    }
  )
)



#' wrapper around R6 method callback for saving epoch optimizer weights
#' @export
callback_save_optimizer_weights <- function() {
  cb <- CallbackOptCheckpoint$new()
  cb
}
