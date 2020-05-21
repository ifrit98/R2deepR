


#' Prepare MNIST dataset for use with a keras model object.
#' @importFrom keras dataset_mnist to_categorical
#' @export
mnist_data <- function() {
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

  list(x_train = x_train, x_test = x_test, y_train = y_train, y_test = y_test)
}



#' Convenience wrapper around `keras::dataset_imdb()` which pads
#' input seqequences for use with a keras model in addition to
#' preprocessing already applied in keras.
#' @importFrom keras dataset_imdb pad_sequences
#' @importFrom reticulate import
#' @export
padded_imdb_data <-
  function(vocab_size = 10000,
           max_length = 150,
           trunc_type = 'post') {

    vocab_size %<>% as.integer()
    max_length %<>% as.integer()

    np <- import("numpy")

    c(train, test) %<-% dataset_imdb(num_words = vocab_size)


    training_sentences <- list()
    training_labels    <- list()
    testing_sentences  <- list()
    testing_labels     <- list()


    for (i in seq_along(train$x)) {
      training_sentences[[i]] <- train$x[[i]]
      training_labels[[i]] <- train$y[[i]]
    }

    for (i in seq_along(test$x)) {
      testing_sentences[[i]] <- test$x[[i]]
      testing_labels[[i]] <- test$y[[i]]
    }

    training_labels_np <- np$array(training_labels, dtype = np$int32)
    testing_labels_np <- np$array(testing_labels, dtype = np$int32)


    training_padded <- pad_sequences(
      training_sentences,
      maxlen = max_length,
      truncating = trunc_type
    )

    testing_padded <-
      pad_sequences(testing_sentences, maxlen = max_length)

    list(training_padded, training_labels_np, testing_padded, testing_labels_np)

  }

