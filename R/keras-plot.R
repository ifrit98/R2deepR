# TODO: Extract what we need from this to make nice learning curve plots!

keras_plot <-
  function (x, # history object (list)
            y, # Unused
            metrics = NULL,
            method = c("auto", "ggplot2",
                       "base"),
            smooth = getOption("keras.plot.history.smooth",
                               TRUE),
            theme_bw = getOption("keras.plot.history.theme_bw",
                                 FALSE),
            ...) {
    browser()
    method <- match.arg(method)
    if (method == "auto") {
      if (requireNamespace("ggplot2", quietly = TRUE))
        method <- "ggplot2"
      else
        method <- "base"
    }
    df <- as.data.frame(x)
    if (is.null(metrics))
      metrics <- Filter(function(name)
        ! grepl("^val_", name),
        names(x$metrics))
    df <- df[df$metric %in% metrics,]
    if (method == "ggplot2") {
      int_breaks <- function(x)
        pretty(x)[pretty(x) %% 1 == 0]
      if (x$params$do_validation) {
        if (theme_bw)
          p <- ggplot2::ggplot(
            df,
            ggplot2::aes_(
              ~ epoch,
              ~ value,
              color = ~ data,
              fill = ~ data,
              linetype = ~ data,
              shape = ~ data
            )
          )
        else
          p <- ggplot2::ggplot(df,
                               ggplot2::aes_(
                                 ~ epoch,
                                 ~ value,
                                 color = ~ data,
                                 fill = ~ data
                               ))
      }
      else {
        p <- ggplot2::ggplot(df, ggplot2::aes_( ~ epoch, ~ value))
      }
      smooth_args <- list(se = FALSE,
                          method = "loess",
                          na.rm = TRUE)
      if (theme_bw) {
        smooth_args$size <- 0.5
        smooth_args$color <- "gray47"
        p <- p + ggplot2::theme_bw() + ggplot2::geom_point(col = 1,
                                                           na.rm = TRUE,
                                                           size = 2) + ggplot2::scale_shape(solid = FALSE)
      }
      else {
        p <- p + ggplot2::geom_point(shape = 21,
                                     col = 1,
                                     na.rm = TRUE)
      }
      if (smooth && x$params$epochs >= 10)
        p <- p + do.call(ggplot2::geom_smooth, smooth_args)
      p <- p + ggplot2::facet_grid(metric ~ ., switch = "y",
                                   scales = "free_y") + ggplot2::scale_x_continuous(breaks = int_breaks) +
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          strip.placement = "outside",
          strip.text = ggplot2::element_text(colour = "black",
                                             size = 11),
          strip.background = ggplot2::element_rect(fill = NA,
                                                   color = NA)
        )
      return(p)
    }
    if (method == "base") {
      op <- par(mfrow = c(length(metrics), 1), mar = c(3, 3,
                                                       2, 2))
      on.exit(par(op), add = TRUE)
      for (i in seq_along(metrics)) {
        metric <- metrics[[i]]
        top_plot <- i == 1
        bottom_plot <- i == length(metrics)
        if (top_plot)
          par(mar = c(1.5, 3, 1.5, 1.5))
        else if (bottom_plot)
          par(mar = c(2.5, 3, 0.5, 1.5))
        else
          par(mar = c(1.5, 3, 0.5, 1.5))
        df2 <- df[df$metric == metric,]
        plot(
          df2$epoch,
          df2$value,
          pch = c(1, 4)[df2$data],
          xaxt = ifelse(bottom_plot, "s", "n"),
          xlab = "epoch",
          ylab = metric,
          ...
        )
        legend_location <- ifelse(df2[df2$data == "training",
                                      "value"][1] > df2[df2$data == "training", "value"][x$params$epochs],
                                  "topright", "bottomright")
        if (x$params$do_validation)
          graphics::legend(legend_location,
                           legend = c(metric,
                                      paste0("val_", metric)),
                           pch = c(1, 4))
        else
          graphics::legend(legend_location, legend = metric,
                           pch = 1)
      }
    }
  }


# LR_ACC  <- 1 # learn rate versus accuracy
# ACC_TSS <- 2 # accuracy vs training set size

# TODO: Make wrapper for this to loop over histories list?
# TODO: control flow for how to plot.  e.g. if LR vs acc, then x = LR, y = acc,
# so not to be in separate panes!
# EXPECTS MODIFIED HISTORY OBJECT (list of lists for each run?)
keras.plot <- function(histories, x_axis = NULL) {
  # hist <- vector("list", length(histories))
  # hist$metrics <- list(
  #   loss = c(),
  #   accuracy = c(),
  #   val_loss = c(),
  #   val_accuracy = c(),
  #   LR = c()
  # )
  #
  # hist$params <-
  #   list(
  #     batch_size = c(),
  #     epochs = c(),
  #     steps = c(),
  #     samples = c(),
  #     verbose = c(),
  #     do_validation = c(),
  #     metrics = c()
  #   )

  if (is.null(x_axis))
    x_axis <- "Training Set Size"

  metrics <- vector("list", length(histories))

  for (i in seq(histories))
    metrics[[i]] <- map(histories[[i]]$metrics, function(x) tail(x, 1))

  x <- bind_rows(metrics)
  h <- histories[[1]]
  h$metrics <- as.list(x)
  h$params$epochs <- length(histories)

  p <- keras_plot_learning_curves(h)
}




as.data.frame.keras_training_history <- function (x, ...) {
  if (tensorflow::tf_version() < "2.2")
    x$metrics <- x$metrics[x$params$metrics]
  values <- x$metrics
  pad <- x$params$epochs - length(values$loss)
  pad_data <- list()
  for (metric in x$params$metrics) pad_data[[metric]] <- rep_len(NA,
                                                                 pad)
  values <- rbind(values, pad_data)
  df <- data.frame(epoch = seq_len(x$params$epochs), value = unlist(values),
                   metric = rep(sub("^val_", "", names(x$metrics)), each = x$params$epochs),
                   data = rep(grepl("^val_", names(x$metrics)), each = x$params$epochs))
  rownames(df) <- NULL
  df$data <- factor(df$data, c(FALSE, TRUE), c("training",
                                               "validation"))
  df$metric <- factor(df$metric, unique(sub("^val_", "", names(x$metrics))))
  df
}

# LR_ACC = 1
# LR_TSS = 2

# TODO: find way to change axis labels easily by argument
plot_learning_curves <-
  function (x,
            y, # Unused
            x_axis_label = NULL,
            x_axis_values = NULL,
            metrics = NULL,
            smooth = getOption("keras.plot.history.smooth", TRUE),
            theme_bw = getOption("keras.plot.history.theme_bw", FALSE),
            ...) {
    requireNamespace("ggplot2", quietly = TRUE)
    df <- as.data.frame(x)
    if (is.null(x_axis_label)) x_axis_label <- "epochs"
    if (is.null(metrics)) metrics <- names(x$metrics)
    # metrics <- Filter(function(name)
    #   ! grepl("^val_", name),
    #   names(x$metrics))
    df <- df[df$metric %in% metrics,]

    int_breaks <- function(x) pretty(x)[pretty(x) %% 1 == 0]

    if (x$params$do_validation) {
      p <- ggplot2::ggplot(df,
                           ggplot2::aes_(
                             ~ epoch, # Substitute x_axis argument here?
                             ~ value, # Substitute y_axis argument here?
                             color = ~ data,
                             fill = ~ data
                           ))
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes_( ~ epoch, ~ value))
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
                                 scales = "free_y") + # modify y_scale here for LR?
      ggplot2::scale_x_continuous(breaks = int_breaks) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        strip.placement = "outside",
        strip.text = ggplot2::element_text(colour = "black",
                                           size = 11),
        strip.background = ggplot2::element_rect(fill = NA,
                                                 color = NA)
      )
    p <- p + ggplot2::labs(x = x_axis_label)
    # TODO: Make sure x_axis_values get set accordingly... blank right now
    # p <- p + scale_x_discrete(breaks = 1:5, labels = x_axis_values)
    return(p)
  }
