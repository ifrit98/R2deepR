# See: http://proceedings.mlr.press/v9/glorot10a/glorot10a.pdf
# Mean - Std of activation values (output of non-linearity) during training,
# for different hidden layers.


# (Fig 1) Activation values (y) for each layer over epochs (x)

# (Fig 3) Top 98 percentiles and standard deviation of the distribution of activation values for t
# for the non-linearity

# (Fig 4) Activation values normlized histogram at the end of training, avereged across
# units of same layer and accross 300 test examples


# (Fig 5) Cross entropy and quadratic cost as a function of two weights (one for each W1, W2)

# TODO: DEFINE CUSTOM METRIC to be tracked with model history
collect_activations <- callback_lambda(on_epoch_end = function(epoch, logs = list()) {
  if (identical(logs[['activations']], NULL)) logs[['activations']] <- list()

  a0 <- model$layers[[1]](x_train[1:128,] %>% deepR::as_tensor(tf$float32))
  a1 <- model$layers[[2]](a0)
  a2 <- model$layers[[3]](a1)
  a3 <- model$layers[[4]](a2)

  layer_activation_means <-
    lapply(list(a1, a2, a3), function(x) {
      df <- x$numpy()
      batch_means <- sapply(1:length(df[,1]), function(i) mean(df[i,]))

      mean(batch_means)
    })

  # TODO: assign in namespace of parent? or super assign <<-?
  append(logs[['activations']], layer_activation_means)
})


# TODO: can we not get access to model in metric callback?
act_metric <- custom_metric("activations", function(y_true, y_pred) {
  browser()
  a0 <- model$layers[[1]](x_train[1:128,] %>% deepR::as_tensor(tf$float32))
  a1 <- model$layers[[2]](a0)
  a2 <- model$layers[[3]](a1)
  a3 <- model$layers[[4]](a2)

  layer_activation_means <-
    lapply(list(a1, a2, a3), function(x) {
      df <- x$numpy()
      batch_means <- sapply(1:length(df[,1]), function(i) mean(df[i,]))

      mean(batch_means)
    })

  layer_activation_means
})


if (FALSE) {

  c(x_train, x_test, y_train, y_test) %<-% deepR::mnist_data()

  input <- layer_input(shape = list(784))

  a <- input %>% layer_dense(64, activation = 'tanh')
  b <- a %>% layer_dense(32, activation = 'tanh')
  c <- b %>% layer_dense(24, activation = 'tanh')


  output <- c %>%
    layer_dense(10, activation = 'softmax')


  model <- keras_model(input, output) %>%
    compile(
      'adam',
      'categorical_crossentropy',
      metrics = c('accuracy')
    )


  hist <- model %>% fit(
    x_train,
    y_train,
    batch_size = 128,
    epochs = 5,
    callbacks = list(epoch_end)
  )

}
