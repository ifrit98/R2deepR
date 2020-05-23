

compute_training_set_sizes <- function(n_elems, type) {
  (scale <- switch(
    type,
    "log" = log(n_elems) %>% floor(),
    "log2" = log(n_elems, base = 2) %>% floor(),
    "linear" = linear_factor
  ))

  inc <- floor(n_elems / scale)

  (train_sizes <- switch(
    type,
    "log" = lapply(1:scale, function(x) floor(e^x)),
    "log2" = lapply(1:scale, function(x) floor(2^x)),
    "linear" = lapply(1:scale, function(x) floor(x * inc))
  ))

  train_sizes
}


# TODO: Update to pass tfdatasets ds objects instead of `x_train`
#' Complete a set of training runs given an integer set of training set sizes.
#'
#' Plots Jtrain and Jcv loss and accuracy curves
#' @export
train_set_size_run <-
  function(model_fn,
           x_train,
           y_train,
           type = "linear",
           epochs = 25,
           n_elems = dim(x_train)[1],
           linear_factor = 10,
           train_sizes = NULL,
           min_train_size = NULL,
           n_runs = NULL) {

    # Calculate training set sizes
    if (is.null(train_sizes))
      train_sizes <- compute_training_set_sizes(n_elems, type)

    cut_index <- max(which(train_sizes < min_train_size)) + 1
    if (!is.finite(cut_index))
      train_sizes <- train_sizes[cut_index:length(train_sizes)]

    # Grab training data
    # Randomly grab indices to emulate shuffling
    n_iter <- length(train_sizes)

    idx <-
      lapply(1:n_iter, function(i)
        sample.int(length(x_train[,1]), train_sizes[[i]]))

    train_sets <-
      lapply(1:n_iter, function(i) x_train[idx[[i]],])

    i <- 1
    histories <- vector("list", n_iter)
    plots     <- vector("list", n_iter)

    for (i in seq(n_iter)) {

      # Reinstantiate model with fresh weights
      model <- model_fn() #R2deepR::ex_model()

      # Grab current training sets
      x_train_i <- x_train[idx[[i]],]
      y_train_i <- y_train[idx[[i]],]

      # Do traning run
      hist <- fit(
        model,
        x_train_i,
        y_train_i,
        validation_data = list(x_test, y_test),
        epochs = epochs
      )

      # TODO: Save pdf of history plot in a subdir
      # TODO: facet history plots so they are on same pdf
      histories[[i]] <- hist
      plots[[i]] <- plot(hist)
    }


    # Only want last values of each history object
    df <- bind_as_cols(
      acc = lapply(histories, function(h) h$metrics$acc %>% last()) %>% unlist(),
      loss = lapply(histories, function(h) h$metrics$loss %>% last()) %>% unlist(),
      val_loss = lapply(histories, function(h) h$metrics$val_loss %>% last()) %>% unlist(),
      val_acc = lapply(histories, function(h) h$metrics$val_acc %>% last()) %>% unlist(),
      train_sizes = train_sizes %>% unlist()
    ) %>% as_tibble()

    h <- histories[[1]]
    h$metrics <- as.list(df)
    h$params$epochs <- length(df$acc)

    p <- plot_curves_from_keras_history(h, "train_set_size", train_sizes)
    ggsave(plot_filename, p)

    p
}




plot_curves_from_keras_history <-
  function (x,
            x_axis_label = NULL,
            x_axis_values = NULL,
            metrics = NULL,
            smooth = getOption("keras.plot.history.smooth", TRUE),
            theme_bw = getOption("keras.plot.history.theme_bw", FALSE),
            ...) {
    requireNamespace("ggplot2", quietly = TRUE)

    df <- as.data.frame(x)
    x_axis_values %<>% unlist()

    if (is.null(x_axis_label))
      x_axis_label <- "epochs"
    if (is.null(metrics))
      metrics <- names(x$metrics)

    x_len <- length(x_axis_values)
    df$epoch <- rep(x_axis_values, length(df$epoch) / x_len)
    df$params$epochs <- x_len

    df <- df[df$metric %in% metrics, ]

    int_breaks <- function(x)
      pretty(x)[pretty(x) %% 1 == 0]

    if (x$params$do_validation) {
      p <- ggplot2::ggplot(df,
                           ggplot2::aes_(~ epoch,
                                         ~ value,
                                         color = ~ data,
                                         fill = ~ data))
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes_(~ epoch, ~ value))
    }
    smooth_args <- list(se = FALSE,
                        method = "loess",
                        na.rm = TRUE)
    p <- p + ggplot2::geom_point(shape = 21,
                                 col = 1,
                                 na.rm = TRUE)
    if (smooth && x$params$epochs >= 5)
      p <- p + do.call(ggplot2::geom_smooth, smooth_args)

    p <- p + ggplot2::facet_grid(metric ~ ., switch = "y",
                                 scales = "free_y") +
      ggplot2::scale_x_log10(breaks = x_axis_values) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        strip.placement = "outside",
        strip.text = ggplot2::element_text(colour = "black",
                                           size = 11),
        strip.background = ggplot2::element_rect(fill = NA,
                                                 color = NA)
      )
    p <- p + ggplot2::labs(x = x_axis_label)

    return(p)
  }




mini_batch_size_run <- function(model_fn,
                                ds,
                                val_ds,
                                batch_sizes = NULL,
                                optimizer = 'adam',
                                loss = 'categorical_crossentropy',
                                metrics = c('accuracy'),
                                test_epochs = 10,
                                min_batch_size = 1,
                                max_batch_size = 1024,
                                train_set_size = 0,
                                return_plots = FALSE) {

  # Set up batch sizes to try
  # TODO: (evenly divide total train set size as default instead of powers of 2)
  if (is.null(batch_sizes))
    batch_sizes <- pow2_up_to(max_batch_size)

  cut_index <- max(which(batch_sizes < min_batch_size)) + 1
  if (is.finite(cut_index))
    batch_sizes <- batch_sizes[cut_index:length(batch_sizes)]


  # Initialize history list
  histories <- vector("list", length = length(batch_sizes))
  # histories <- JSGutils:::nlist(1:length(batch_sizes))


  # Run training on several models
  # (up to a threshold, with callbacks for early stopping, etc)
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

  # TODO: Facet all plots on one and save to PDF
  all_hist_plots <- lapply(histories, plot)


  df <- map2(batch_sizes, histories, function(x, y) {
    batch_size <- rep(x, test_epochs)

    df  <- y$metrics %>% as_tibble()
    tbl <- tibble(batch_size)

    bind_cols(df, tbl)
  }) %>% dplyr::bind_rows()

  # Only want last values of each history object
  df <- bind_as_cols(
    acc = lapply(histories, function(h) h$metrics$acc %>% last()) %>% unlist(),
    loss = lapply(histories, function(h) h$metrics$loss %>% last()) %>% unlist(),
    val_loss = lapply(histories, function(h) h$metrics$val_loss %>% last()) %>% unlist(),
    val_acc = lapply(histories, function(h) h$metrics$val_acc %>% last()) %>% unlist(),
    batch_sizes = batch_sizes
  ) %>% as_tibble()

  h <- histories[[1]]
  h$metrics <- as.list(df)
  h$params$epochs <- length(df$acc)

  (p <- plot_curves_from_keras_history(h, x_axis_label = "batch_size", x_axis_values = batch_sizes))
  ggsave(plot_filename, p)


  vacc <- df$val_acc
  names(vacc) <- batch_sizes
  best_batch_size <- as.integer(which(vacc == max(vacc)) %>% names())


  if (return_plots)
    return(list(
      batch_plot = p,
      hist_plots = all_hist_plots,
      batch_size = best_batch_size
    ))
  else
    return(best_batch_size)
}



if (FALSE) {

devtools::load_all()
model_fn <- R2deepR::ex_model

c(x_train, x_test, y_train, y_test) %<-% R2deepR::mnist_data()

ds <-
  tensor_slices_dataset(tuple(x_train, y_train)) %>%
  dataset_shuffle(1000) %>%
  dataset_batch(128, drop_remainder = TRUE)

val_ds <-
  tensor_slices_dataset(tuple(x_test, y_test)) %>%
  dataset_shuffle(1000) %>%
  dataset_batch(128, drop_remainder = TRUE)

best_batch_size <-
  mini_batch_size_run(model_fn, test_epochs = 2, max_batch_size = 16)

best_train_size <-
  train_set_size_run(model_fn, x_train = x_train, y_train = y_train)

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
