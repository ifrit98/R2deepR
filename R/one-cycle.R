
#' @export
compute_momentum <- function(
  iter, mid_cycle_id=50,
  max_momentum = 0.99,
  min_momentum = 0.1
) {
  if (iter > (2 * mid_cycle_id)) {
    momentum <- max_momentum
  } else if (iter > mid_cycle_id) {
    current_percent <- 1 - ((iter - mid_cycle_id) / mid_cycle_id)
    momentum <- max_momentum  - current_percent * (
      max_momentum - min_momentum)
  } else {
    current_percent <- iter / mid_cycle_id
    momentum <- max_momentum  - current_percent * (
      max_momentum - min_momentum)
  }
}


#' @export
compute_lr <- function(iter = 1) {

  # Scale must be fixed at .1 for this algorithm to work properly?
  self <- list(
    mid_cycle_id = 100,
    max_iter = 200,
    iter = iter,
    init_lr = 1e-4,
    max_lr = 4e-1,
    scale = 0.1
  )

  # TODO: Shouldn't this be based on max_lr and not iters?
  # TODO: compute mid_cycle_id to line up with max_lr
  self$init_lr <- self$max_lr

  if (iter > (2 * self$mid_cycle_id)) {

    current_percent <-
      (self$iter - (2 * self$mid_cycle_id)) /
      (self$num_iterations - 2 * self$mid_cycle_id)

    new_lr <- self$init_lr * (1 + (current_percent * (1 - 100) / 100)) * self$scale
  } else if (self$iter > self$mid_cycle_id) {

    current_percent <-
      1 - (self$iter - self$mid_cycle_id) / self$mid_cycle_id
    new_lr <-
      self$init_lr * (1. + current_percent * (self$scale * 100 - 1)) * self$scale

  } else {
    current_percent <- self$iter / self$mid_cycle_id
    new_lr <-
      self$init_lr * (1 + current_percent * (self$scale * 100 - 1)) * self$scale
  }

  if (self$iter == self$max_iter)
    self$iter <- 0

  new_lr
}



#' @export
OneCycleCallback <- R6::R6Class("OneCycleCallback",

  inherit = KerasCallback,

  public = list(

    init_lr = NULL,
    max_lr = NULL, # init_lr
    end_percent = NULL,
    scale = NULL, # 0.1
    max_momentum = NULL, #0.95,
    min_momentum = NULL, #0.85,
    update_momentum = NULL,
    verbose = TRUE,
    epochs = NULL,
    steps_per_epoch = NULL,
    batch_size = NULL,
    samples = NULL,
    steps = NULL,
    num_iter = NULL,
    mid_cycle_id = NULL,
    lr_history = NULL,
    momentum_history = NULL,
    log_history = NULL,
    iter = NULL,
    max_iter = NULL,

    initialize = function(
      max_lr,
      end_percent,
      scale,
      max_momentum,
      min_momentum,
      update_momentum,
      verbose,
      steps_per_epoch,
      num_iter,
      mid_cycle_id
    ) {

      if (end_percent < 0 || end_percent > 1)
        stop("`end_percentage` must be between 0 and 1")

      self$init_lr      <- max_lr
      self$end_percent  <- end_percent
      self$max_momentum <- max_momentum
      self$min_momentum <- min_momentum
      self$scale        <- scale
      self$num_iter     <- num_iter
      self$mid_cycle_id <- mid_cycle_id
      self$end_percent  <- end_percent
      self$verbose      <- verbose
      self$steps_per_epoch <- steps_per_epoch

      self$update_momentum <-
        if (!is.null(self$max_momentum) && !is.null(self$min_momentum))
          TRUE else FALSE

      self$iter <- 0
      self$lr_history <- vector("list", num_iter)
      self$log_history <- list()
      self$momentum_history <- vector("list", num_iter)
    },

    compute_lr = function() {

      stopifnot(self$iter <= self$max_iter)
      h
      if (self$iter > (2 * self$mid_cycle_id)) {
        current_percent <-
          (self$iter - (2 * self$mid_cycle_id)) /
          (self$num_iter - 2 * self$mid_cycle_id)

        new_lr <-
          self$init_lr * (1 + (current_percent * (1 - 100) / 100)) * self$scale

      } else if (self$iter > self$mid_cycle_id) {
        current_percent <-
          1 - (self$iter - self$mid_cycle_id) / self$mid_cycle_id

        new_lr <-
          self$init_lr * (1 + current_percent * (self$scale * 100 - 1)) * self$scale

      } else {
        current_percent <- self$iter / self$mid_cycle_id
        new_lr <-
          self$init_lr * (1 + current_percent * (self$scale * 100 - 1)) * self$scale
      }

      if (self$iter == self$max_iter)
        self$iter <- 0

      new_lr
    },

    compute_momentum = function() {
      if (self$iter > (2 * self$mid_cycle_id)) {
        momentum <- self$max_momentum
      } else if (self$iter > self$mid_cycle_id) {
        current_percent <- 1 - ((self$iter - self$mid_cycle_id) / self$mid_cycle_id)
        momentum <- self$max_momentum  - current_percent * (
          self$max_momentum - self$min_momentum)
      } else {
        current_percent <- self$iter / self$mid_cycle_id
        momentum <- self$max_momentum  - current_percent * (
          self$max_momentum - self$min_momentum)
      }

      momentum
    },

    .reset = function() {
      self$lr_history <- vector("list", self$num_iter)
      self$log_history <- vector("list", self$num_iter)
      self$momentum_history <- vector("list", self$num_iter)
      self$iter <- 0
    },

    on_epoch_end = function(epoch, logs = NULL) {
      lr <- tf$keras$backend$get_value(self$model$optimizer$learning_rate)
      momentum <- tf$keras$backend$get_value(self$model$optimizer$momentum)
      if (self$verbose) {
        if (self$update_momentum)
          cat(" - lr: %d  - momentum: %d \n",
              tf$keras$backend$get_value(lr),
              tf$keras$backend$get_value(momentum), "\n")
      } else
        cat(" - lr: %d ", lr, "\n")
    },

    on_batch_end = function(batch, logs = NULL) {

      logs <- if (!is.null(logs)) logs else list()

      self$iter <- self$iter + 1
      new_lr <- self$compute_lr()

      self$lr_history[[self$iter]] <-
        tf$keras$backend$get_value(self$model$optimizer$lr)

      tf$keras$backend$set_value(self$model$optimizer$lr, new_lr)

      # TODO: Fix learn rate append
      # lrs <- self$model$history$history$learn_rate
      # if(batch >= 467)
      #   browser()
      # self$model$history$history$learn_rate <- c(lrs, new_lr)

      if (self$update_momentum) {
        if (!'momentum' %in% names(self$model$optimizer$get_config()))
          stop("Momentum can be updated only with Adam or SGD optimizers")

        self$momentum_history[[self$iter]] <-
          tf$keras$backend$get_value(self$model$optimizer$momentum)

        new_momentum <- self$compute_momentum()
        tf$keras$backend$set_value(self$model$optimizer$momentum, new_momentum)
      }

      append(self$log_history, logs)

    },

    on_train_begin = function(logs = NULL) {

      # TODO: Fix history learn rate append op
      # metrics <- self$model$history$params$metrics
      # self$model$history$params$metrics <<- c(metrics, "learning_rate")

      logs <- if (!is.null(logs)) logs else list()

      self$epochs     <- self$params$epochs
      self$batch_size <- self$params$batch_size
      self$samples    <- self$params$samples
      self$steps      <- self$params$steps
      self$max_iter   <- self$epochs * self$steps_per_epoch + 1

      self$mid_cycle_id <-
        as.integer(self$max_iter * ((1 - self$end_percent)) / 2)

      self$.reset()

      new_lr <- self$compute_lr()
      tf$keras$backend$set_value(self$model$optimizer$lr, new_lr)

      if (self$update_momentum) {
        if (!'momentum' %in% names(self$model$optimizer$get_config()))
          stop("Momentum can be updated only with SGD optimizer")

        new_momentum <- self$compute_momentum()
        tf$keras$backend$set_value(self$model$optimizer$momentum, new_momentum)
      }
    }
  )
)


one_cycle_callback <- function(max_lr,
                               steps_per_epoch,
                               num_iter,
                               end_percent = 0.1,
                               scale = 0.1,
                               max_momentum = 0.95,
                               min_momentum = 0.85,
                               update_momentum = TRUE,
                               verbose = TRUE,
                               mid_cycle_id = NULL) {

  OneCycleCallback$new(
    max_lr = max_lr,
    end_percent = end_percent,
    max_momentum = max_momentum,
    min_momentum = min_momentum,
    scale = scale,
    verbose = verbose,
    steps_per_epoch = steps_per_epoch,
    num_iter = num_iter,
    mid_cycle_id = mid_cycle_id
  )
}

# TODO: on_train_end (or like we have with hist obj in add_lr_hist callback)
# add the learn rate so that it shows up in the returned keras history object from fit()

if (FALSE) {
  library(deepR)
  library(tensorflow)
  library(tfdatasets)
  # devtools::load_all()


  oc <- one_cycle_callback(max_lr = 4e-1, steps_per_epoch = 468, num_iter = 1000)

  # debugonce(oc$on_train_begin)
  # debugonce(oc$on_epoch_begin)
  # debugonce(oc$on_epoch_end)
  # debugonce(oc$on_batch_begin)
  # debugonce(oc$on_batch_end)

  c(ds, val_ds) %<-% mnist_dataset()

  model <- ex_model(optimizer = 'SGD')

  h <- model %>%
    fit(
      ds,
      epochs = 2,
      callbacks = list(oc)
    )

}

