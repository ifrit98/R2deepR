# Bengaliai: https://www.kaggle.com/h030162/how-to-get-score-of-0-98-with-one-gtx-1080ti

# Recommender systems: https://towardsdatascience.com/introduction-to-recommender-systems-6c66cf15ada

# Learning curves
# 10.4 diagnosing bias-variance: https://www.youtube.com/watch?v=fDQkUN9yw44&list=PLLssT5z_DsK-h9vYZkQkYNWcItqhlRJLN&index=61
# 10.6 learning curves https://www.youtube.com/watch?v=ISBGFY-gBug&list=PLLssT5z_DsK-h9vYZkQkYNWcItqhlRJLN&index=63
# 11.4 precision-recall: https://www.youtube.com/watch?v=W5meQnGACGo&list=PLLssT5z_DsK-h9vYZkQkYNWcItqhlRJLN&index=68


# Assume SGD with momentum for optimizer for parameter grid search?
#

if (FALSE) {


  model <- R2deepR::ex_model()

  c(x_train, x_test, y_train, y_test) %<-% deepR::mnist_data()

  epochs <- 10
  LR <- vector("list", epochs)


  collect_lr <- callback_lambda(on_epoch_end = function(epoch, logs) {
    lr <- model$optimizer$get_config()$learning_rate
    LR[[epoch+1]] <<- lr
  })

  hist <- model %>% fit(
    x_train,
    y_train,
    batch_size = 128,
    epochs = epochs,
    callbacks = list(collect_lr)
  )



  # Plot scheme 1: Jtrain + Jcv as function of training set size



  # For loop around fit?
  # Callback?

  # TODO: Generalize for other plot types?  Collect all/some info and then separate out to
  # postprocess in different functions?
  train_set_size_run <-
    function(model,
             df,
             type = "linear",
             n_elems = dim(df)[1],
             linear_factor = 10,
             train_sizes = NULL,
             n_runs = NULL) {

      df <- x_train
      n_elems <- dim(df)[1]
      type <- "log"
      train_sizes <- NULL

      # Calculate training set sizes
      (scale <- switch(
        type,
        "log" = log(n_elems) %>% floor(),
        "log2" = log(n_elems, base = 2) %>% floor(),
        "linear" = linear_factor
      ))

      inc <- floor(n_elems / scale)

      if (is.null(train_sizes)) {
        (train_sizes <- switch(
          type,
          "log" = lapply(1:scale, function(x) floor(e^x)),
          "log2" = lapply(1:scale, function(x) floor(2^x)),
          "linear" = lapply(1:scale, function(x) floor(x * inc))
        ))
      }

      train_sizes[c(6,7,8,9,10,11)] <- NULL

      # Grab training data
      # Randomly grab indices to emulate shuffling
      n_iter <- length(train_sizes)
      idx <-
        lapply(1:n_iter, function(i) sample.int(length(df[,1]), train_sizes[[i]]))

      train_sets <- lapply(1:n_iter, function(i) df[idx[[i]],])
      # train_set <- df[idx[[i]],]

      i <- 1
      histories <- vector("list", n_iter)
      plots     <- vector("list", n_iter)
      for (i in seq(n_iter)) {
        # Reinstantiate model with fresh weights
        model <- deepR::ex_model()
        LR <- list("list", epochs)

        # Grab current training sets
        x_train_i <- df[idx[[i]],] #train_sets[[i]]
        y_train_i <- y_train[idx[[i]],]

        # Do traning run
        hist <- fit(
          model,
          x_train_i,
          y_train_i,
          validation_data = list(x_test, y_test),
          callbacks = list(collect_lr),
          epochs = 10
        )

        # Save pdf of history plot in a subdir?

        # Collect history and plots
        hist$metrics$LR <- unlist(LR)
        # Add LR to hist$metrics so conversion to data.frame succeeds
        hist$params$metrics <- c(hist$params$metrics, "LR")
        histories[[i]] <- hist
        p <- plot(hist)
        plots[[i]] <- p
      }


      # control flow for different types of metric comparisions
      # Collect and process results
      accs <- lapply(histories, function(hist) hist$metrics$accuracy[[epochs]])
      val_accs <- lapply(histories, function(hist) hist$metrics$val_accuracy[[epochs]])

      # TODO: plot with keras plot dispatch method? (or write your own)
      plot(
        1:n_iter,
        accs,
        type = "b",
        xlab = "train_set_size",
        axes = FALSE,
        sub = paste0("number of epochs: ", epochs)
      )
      # TODO: add axes for each plot?
      axis(1, 1:n_iter, labels = train_sizes)
      axis(2, seq(0, 1, 0.001))
      # TODO: Add epochs legend
      epochs    <- histories[[i]]$params$epochs
      best_acc  <- histories[[i]]$metrics$accuracy[[epochs]]
      best_loss <- histories[[i]]$metrics$loss[[epochs]]


    }

}



# mini_batch_size_run <-
  function(model_fn = deepR::ex_model(),
                                optimizer = 'adam',
                                loss = 'categorical_crossentropy',
                                metrics = c('accuracy'),
                                test_epochs = 10,
                                min_batch_size = 1,
                                max_batch_size = 1024,
                                train_set_size = 0) {
  devtools::load_all()

  # model <- deepR::ex_model()
  model_fn <- R2deepR::ex_model
  model <- model_fn()

  c(x_train, x_test, y_train, y_test) %<-% R2deepR::mnist_data()

  ds <-
    tensor_slices_dataset(tuple(x_train, y_train)) %>%
    dataset_shuffle(1000) %>%
    dataset_batch(128, drop_remainder = TRUE)

  val_ds <-
    tensor_slices_dataset(tuple(x_test, y_test)) %>%
    dataset_shuffle(1000) %>%
    dataset_batch(128, drop_remainder = TRUE)

  # Get mod, LCD, and GCD for train_set_size if nonzero
  divisors <- JSGutils::get_divisors(train_set_size)

  # Set up batch sizes to try (think about total train set size as well)
  batch_sizes <- JSGutils::pow2_up_to(max_batch_size)

  # Initialize history list
  histories <- vector("list", length = length(batch_sizes))
  # histories <- JSGutils:::nlist(1:length(batch_sizes))


  # Run training on several models (up to a threshold, with callbacks for early stopping, etc)
  for (i in seq.int(1, length(batch_sizes))) {

    # Instantiate new model
    model <- model_fn()

    # Reset the model
    model %>% compile(
      optimizer = optimizer,
      loss = loss,
      metrics = metrics
    )

    print(paste("Training batch size:", batch_sizes[i]))

    # Train and store history object
    histories[[i]] <- model %>%
      fit(
        ds,
        validation_data = val_ds,
        epochs = test_epochs,
        verbose = 1
      )

  }

  names(histories) <- batch_sizes


  df <- map2(batch_sizes, histories, function(x, y) {
    batch_size <- rep(x, test_epochs)

    df  <- y$metrics %>% as_tibble()
    tbl <- tibble(batch_size)

    bind_cols(df, tbl)
  }) %>% bind_rows()

  # Just want the last values!
  val_accs <-
    lapply(histories, function(h) h$metrics$val_accuracy %>% tail(1)) %>%
    unname() %>%
    unlist()

  accs <-
    lapply(histories, function(h) h$metrics$accuracy %>% tail(1))

  losses <-
    lapply(histories, function(h) h$metrics$loss %>% tail(1))

  val_losses <-
    lapply(histories, function(h) h$metrics$val_loss %>% tail(1))

  df <- bind_as_cols(val_accs, batch_sizes)
  colnames(df) <- c("val_acc", "batch_size")
  df %<>% as_tibble()


  dl <- lapply(histories, function(h) tibble::as_tibble(h$metrics))
  df <- listarrays::bind_as_cols(dl)

  bsr <- batch_sizes_rep <- lapply(batch_sizes, function(bs) rep(bs, test_epochs))
  bs <- tibble(batch_sizes)

  ggplot(df, aes(batch_size, val_acc)) + geom_point() + scale_x_log10()
  plot(histories[[1]])

  # Compare results on validation data
  # Return best batch size (int)


}


## TODO: Naive random grid search implementation for n values:
# (lr, momentum, batch, # hidden_units & # layers (complexity), learn_rate_decay)
# use appropriate scale for random sampling within each hparam (log vs linear scale)
# e.g. r = -4 * np.random.rand()
#      alpha = 10^r
#  a = log10(lo) ##  log10(0.00001)  = - 4
#  b = log10(hi) ##  log10(1) = 0
#  r -> [a, b]
#  r_lo = 10 ^ (np.random.rand() * a)
#  r_hi = 10 ^ (np.random.rand() * b)
#

# TODO: Do the same for exponentially weighted averages (Andrew Ng course 2 week 2)
#' Randomly sample values for learning rate parameter `alpha` on a log scale
#'
#' Computes minimum values on a log scale as `a <- log(lo, 10)`, and
#'  `b <- log(hi, 10)`.  Calls to `runif()` are scaled to the appropriate
#'  range by `a` and are checked to be out of bounds against `-b`.  Values
#'  of `alpha` are computed by raising `10` to the sampled value power.
#'
#'
#' @param n_grid_points integer, number of values to sample
#' @param lo minimum learning rate value to return
#' @param hi maximum learning rate value to return
#' @return vector of `alpha` samples within range [log(lo, 10), log(hi, 10)]
#' Potentially run this after the lr_range_test to narrow?
sample_lr_values_log_scale <- function(n_grid_points = 10, lo = 1e-7, hi = 1) {
  f <- function(a) { runif(1) * a }

  sapply(seq(n_grid_points), function(x) {
    a <- log(lo, 10)
    b <- log(hi, 10)

    # Ensure `r` is within proper range [a, b]
    while ({r <- f(a)} > -b) { r <- a * runif(1) }

    alpha <- 10 ^ r

    alpha
  })
}

##
#
# YOU SHOULD JOIN!
#
# Do what Matt's doing with music with engineering.
#
# Dive in to the deep end.
#
# Morning - Noon - Night (wake up excited to get further and appreciate great code [tunes])
#
##
##
#
# If you don't listen to it, it'll keep getting worse and it will keep happening to you.
#
# Stop trying to control things that are outside of your sphere of influence.
# Think too much, reduce the thinking and do more of the doing.
#
##
