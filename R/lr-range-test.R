


#' @importFrom qs qsave
#' @importFrom keras callback_learning_rate_scheduler fit
#' @export
lr_range_test <-
  function(model,
           dataset,
           epochs = 100,
           INIT_LR = 1e-8,
           MAX_LR = 1,
           factor = 10,
           schedule_type = "b",
           plot = TRUE,
           save_history = FALSE,
           history_path = "data/lr-range-test-history.qs") {

    # Schedule A needs at least 125 epochs to reach a high lr
    lr_schedule_a <- callback_learning_rate_scheduler(
      schedule = function(epoch, lr) {
        cat(sprintf("\nEpoch %s: Learning rate: %s\n", epoch, lr))
        INIT_LR * 10 ** (epoch / 20)
      }
    )

    lr_schedule_b <- callback_learning_rate_scheduler(
      schedule =
        function(epoch, lr) {
          cat(sprintf("\nEpoch %s: Learning rate: %s\n", epoch, lr))
          q <- (MAX_LR / INIT_LR) ^ (1 / (epochs - 2))
          INIT_LR * q ^ epoch
        }
    )

    schedule_type <-
      match.arg(schedule_type, c("a", "b"))

    lr_callback <- switch(
      schedule_type,
      a = lr_schedule_a,
      b = lr_schedule_b
    )

    if ("tensorflow.python.keras.engine.sequential.Sequential" %in% class(model))
      fit <- keras:::fit.keras.engine.training.Model

    hist <- model %>% fit(
      dataset,
      epochs = epochs,
      callbacks = list(lr_callback),
      view_metrics = TRUE
    )

    if (save_history)
      qsave(hist, paste0(history_path, '.qs'))

    if (plot)
      plot_lr_range_test_from_hist(hist, max_lr = MAX_LR)

    infer_best_lr_params(hist, factor)
  }



#' @importFrom ggplot2 ggplot geom_line geom_point ggsave
#' @importFrom dplyr bind_cols
#' @importFrom rlang is_empty
#' @export
plot_lr_range_test_from_hist <-
  function(history,
           filename = "learn_rate_range_test",
           max_loss = 10,
           max_lr = 3) {

    loss <- history$metrics$loss
    lr   <- history$metrics$lr

    cut_index <- which.min(match(loss > max_loss, TRUE))
    if (!is_empty(cut_index)) {
      loss[cut_index] <- max_loss
      loss <- loss[1:cut_index]
      lr   <- lr[1:cut_index]
    }

    lr_cut_index <- which.min(match(lr > max_lr, TRUE))
    if (!is_empty(lr_cut_index)) {
      lr[lr_cut_index] <- 3
      lr <- lr[1:lr_cut_index]
      loss <- loss[1:lr_cut_index]
    }

    df <- bind_cols(lr = lr, loss = loss)

    ggplot(df, aes(lr, loss)) +
      geom_line() +
      geom_point() +
      ggsave(file=paste0(filename, ".pdf"), width=8, height=4, dpi=600)
  }



#' @export
infer_best_lr_params <- function(history, factor = 6) {
  loss <- history$metrics$loss
  min_loss <- min(loss)
  idx <- which(loss == min_loss)
  best_run_lr <- history$metrics$lr[[idx]]

  max_lr <- best_run_lr
  min_lr <- max_lr / factor


  c(min_lr, max_lr)
}






if (FALSE) {
  library(deepR)
  library(dplyr)
  library(ggplot2)
  library(magrittr)
  library(zeallot)
  library(tfdatasets)
  library(reticulate)

  devtools::load_all()
  c(x_train, x_test, y_train, y_test) %<-% mnist_data()

  ds <-
    tensor_slices_dataset(tuple(x_train, y_train)) %>%
    dataset_shuffle(1000) %>%
    dataset_batch(128, drop_remainder = TRUE)

  val_ds <-
    tensor_slices_dataset(tuple(x_test, y_test)) %>%
    dataset_shuffle(1000) %>%
    dataset_batch(128, drop_remainder = TRUE)

  model <- ex_model()


  epochs <- 25

  c(init_lr, max_lr) %<-%
    lr_range_test(model, ds, epochs = epochs, schedule_type = "b")

  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(lr = min_lr),
    metrics = c('accuracy')
  )

  hist <- model %>%
    fit(
      ds,
      epochs = epochs,
      validation_data = val_ds
    )

  plot(hist)

}
