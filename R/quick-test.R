

#' Dummy 3 layer dense model to quickly test on mnist (or other) data
#' @export
ex_model <- function(input_shape = list(784), compile = TRUE) {
  input <- keras::layer_input(shape = input_shape)

  a <- input %>% keras::layer_dense(64, activation = 'tanh')
  b <- a %>% keras::layer_dense(32, activation = 'tanh')
  c <- b %>% keras::layer_dense(24, activation = 'tanh')


  output <- c %>%
    keras::layer_dense(10, activation = 'softmax')


  model <- keras::keras_model(input, output)

  if (!exists("FLAGS")) env.utils::import_from("R/flags.R", FLAGS)

  optimizer <-
    keras::optimizer_sgd(
      lr = FLAGS$init_lr,
      momentum = FLAGS$momentum,
      decay = FLAGS$decay,
      nesterov = FLAGS$nesterov, clipnorm = FLAGS$clipnorm,
      clipvalue = FLAGS$clipvalue
    )

  OPT <<- optimizer

  if (compile) model %<>%
    keras::compile(
      optimizer = optimizer,# 'adam',
      'categorical_crossentropy',
      metrics = c('accuracy'))

  model
}



#' Trains a simple deep NN on the MNIST dataset.
#'
#' Gets to 98.40% test accuracy after 20 epochs (there is *a lot* of margin for
#' parameter tuning).
#' @importFrom tensorflow flags
#' @importFrom keras to_categorical keras_model_sequential dataset_mnist
test_mnist_mlp <- function() {

  library(keras)

  # Hyperparameter flags ---------------------------------------------------

  FLAGS <- flags(
    flag_numeric("dropout1", 0.4),
    flag_numeric("dropout2", 0.3)
  )

  # Data Preparation ---------------------------------------------------

  # The data, shuffled and split between train and test sets
  mnist <- dataset_mnist()
  x_train <- mnist$train$x
  y_train <- mnist$train$y
  x_test <- mnist$test$x
  y_test <- mnist$test$y

  # Reshape
  dim(x_train) <- c(nrow(x_train), 784)
  dim(x_test) <- c(nrow(x_test), 784)

  # Transform RGB values into [0,1] range
  x_train <- x_train / 255
  x_test <- x_test / 255

  # Convert class vectors to binary class matrices
  y_train <- to_categorical(y_train, 10)
  y_test <- to_categorical(y_test, 10)

  # Define Model --------------------------------------------------------------

  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
    layer_dropout(rate = FLAGS$dropout1) %>%
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = FLAGS$dropout2) %>%
    layer_dense(units = 10, activation = 'softmax')

  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(lr = 0.001),
    metrics = c('accuracy')
  )

  # Training & Evaluation ----------------------------------------------------

  history <- model %>% fit(
    x_train, y_train,
    batch_size = 128,
    epochs = 20,
    verbose = 1,
    validation_split = 0.2
  )

  plot(history)

  score <- model %>% evaluate(
    x_test, y_test,
    verbose = 0
  )

  cat('Test loss:', score$loss, '\n')
  cat('Test accuracy:', score$acc, '\n')


}
