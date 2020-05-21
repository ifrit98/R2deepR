# TODO: Make residual keras layer wrappers






#' Resnet block bottleneck from the original paper (1512.03385)
#' @references https://arxiv.org/abs/1512.03385
#' @importFrom keras layer_conv_1d layer_add activation_relu
#' @importFrom magrittr %<>% %>%
#' @export
resblock_bottle_vanilla <- function(input, blocks = 1L, filters = c(32, 128)) {

  stopifnot(is_scalar_integerish(blocks))
  blocks %<>% as.integer()

  base <- input

  for(i in 1:blocks) {
    base %<>%
      layer_conv_1d(filters[1], 1, activation = 'relu', padding = 'same') %>%
      layer_conv_1d(filters[1], 3, activation = 'relu', padding = 'same') %>%
      layer_conv_1d(filters[2], 1, padding = 'same') %>%
      {layer_add(list(., layer_conv_1d(base, filters[2], 1)))} %>%
      activation_relu()
  }
  base
}



#' Specialized Residual unit which contains a linear projection layer up-front with
#' two conv blocks with residual connections between before a maxpool
#' @references https://arxiv.org/abs/1906.04459
#' @export
#' @importFrom keras layer_conv_1d layer_max_pooling_1d layer_add
resblock_1d <- function(x, filters, kernel_size) {

  projection <- x %>%
    layer_conv_1d(filters, kernel_size = 1, padding = 'same')

  conv1 <- projection %>%
    layer_conv_1d(filters, kernel_size, padding = 'same', activation = 'relu') %>%
    layer_conv_1d(filters, kernel_size, padding = 'same')

  add1  <- layer_add(list(projection, conv1))

  conv2 <- add1 %>%
    layer_conv_1d(filters, kernel_size, padding = 'same', activation = 'relu') %>%
    layer_conv_1d(filters, kernel_size, padding = 'same')

  out  <- list(conv2, add1) %>%
    {layer_add(.)} %>%
    layer_max_pooling_1d()

  out
}


#' Specialized Residual unit which contains a linear projection layer up-front with
#' two conv blocks with residual connections between before a maxpool
#' @references https://arxiv.org/abs/1906.04459
#' @export
#' @importFrom keras layer_conv_1d layer_max_pooling_1d layer_add
resblock_2d <- function(x, filters, kernel_size = c(3L, 3L), strides = c(2L, 1L)) {

  projection <- x %>%
    layer_conv_2d(filters, kernel_size = 1, padding = 'same')

  conv1 <- projection %>%
    layer_conv_2d(filters, kernel_size, padding = 'same', activation = 'relu') %>%
    layer_conv_2d(filters, kernel_size, padding = 'same')

  add1  <- layer_add(list(projection, conv1))

  conv2 <- add1 %>%
    layer_conv_2d(filters, kernel_size, padding = 'same', activation = 'relu') %>%
    layer_conv_2d(filters, kernel_size, padding = 'same')

  out  <- list(conv2, add1) %>%
    {layer_add(.)} %>%
    layer_max_pooling_2d()

  out
}


#' batchnorm version of `resblock_1d()`
#' @references https://arxiv.org/abs/1906.04459
#' @export
resblock_batchnorm_1d <-
  function(x, filters, kernel_size = 3L, downsample = TRUE) {
    conv1 <- layer_conv_1d(x, filters, kernel_size = 1, padding = 'same')
    conv2 <- layer_conv_1d(conv1, filters, kernel_size,
                           padding = 'same', activation = 'relu')

    bn1 <- layer_batch_normalization(conv2)

    conv3 <- layer_conv_1d(bn1, filters, kernel_size, padding = 'same')
    add1  <- layer_add(list(conv1, conv3))

    conv4 <- layer_conv_1d(conv3, filters, kernel_size,
                           padding = 'same', activation = 'relu')
    conv5 <- layer_conv_1d(conv4, filters, kernel_size, padding = 'same')
    bn2   <- layer_batch_normalization(conv5)
    add2  <- layer_add(list(add1, bn2))

    if(downsample)
      output <- layer_max_pooling_1d(add2)
    else
      output <- add2

    output
  }


#' As suggsted in original batchnorm paper; computes `g(BN(Wx + b))`
#' @references https://arxiv.org/abs/1502.03167
#' @export
resblock_batchnorm_1d_v2 <-
  function(x, filters, kernel_size = 3L, downsample = TRUE) {
    conv1 <- layer_conv_1d(x, filters, kernel_size = 1, padding = 'same')
    conv2 <- layer_conv_1d(conv1, filters, kernel_size,
                           padding = 'same')
    bn1 <- layer_batch_normalization(conv2) %>%
      layer_activation_relu()

    conv3 <- layer_conv_1d(bn1, filters, kernel_size, padding = 'same')
    add1  <- layer_add(list(conv1, conv3))

    conv4 <- layer_conv_1d(conv3, filters, kernel_size,
                           padding = 'same', activation = 'relu')
    conv5 <- layer_conv_1d(conv4, filters, kernel_size, padding = 'same')
    bn2   <- layer_batch_normalization(conv5)
    add2  <- layer_add(list(add1, bn2)) %>%
      layer_activation_relu()

    if(downsample)
      output <- layer_max_pooling_1d(add2)
    else
      output <- add2

    output
  }


#' base unit resblock with batchnorm from Exploring Normalization FB paper (01/2017)
#' @export
resblock_batchnorm_1d_base <-
  function(x, filters, kernel_size = 3L) {
    conv1 <- layer_conv_1d(x, filters, kernel_size, padding = 'same')

    batchnorm <- conv1 %>%
      layer_batch_normalization() %>%
      layer_activation_relu() %>%
      layer_conv_1d(filters, kernel_size, padding = 'same') %>%
      layer_batch_normalization()

    output <- batchnorm %>%
      {layer_add(list(., conv1))} %>%
      layer_activation_relu()

    output
  }


#' base unit resblock with batchnorm from Exploring Normalization FB paper (01/2017)
#' @export
resblock_batchnorm_2d_base <-
  function(x, filters, kernel_size = c(3L, 3L)) {
    conv1 <- layer_conv_2d(x, filters, kernel_size, padding = 'same')

    batchnorm <- conv1 %>%
      layer_batch_normalization() %>%
      layer_activation_relu() %>%
      layer_conv_2d(filters, kernel_size, padding = 'same') %>%
      layer_batch_normalization()

    output <- batchnorm %>%
      {layer_add(list(., conv1))} %>%
      layer_activation_relu()

    output
  }

#' resblock with bottleneck from the Exploring Normalization FB paper (01/2017)
#' @export
resblock_batchnorm_bottle_1d <-
  function(x, filters = 64L, kernel_size = 3L, downsample = TRUE) {
    a <- x %>%
      layer_conv_1d(filters, 1, padding = 'same') %>%
      layer_batch_normalization() %>%
      layer_activation_relu() %>%
      layer_conv_1d(filters, kernel_size, padding = 'same') %>%
      layer_batch_normalization() %>%
      layer_activation_relu()

    channels <- x$shape[-1L] %>% as.character %>%  as.integer

    output <- layer_conv_1d(a, channels, 1, padding = 'same') %>%
      layer_batch_normalization() %>%
      {layer_add(list(x, .))} %>%
      layer_activation_relu()

    output
  }


#' resblock with bottleneck from the Exploring Normalization FB paper (01/2017)
#' modified as in: https://www.kaggle.com/h030162/version1-0-9696
#' @export
resblock_batchnorm_bottle_1d_v2 <-
  function(x, filters = 64L, kernel_size = 3L, downsample = TRUE, renorm = FALSE) {

    a <- x %>%
      layer_conv_1d(filters, 1, padding = 'same') %>%
      layer_batch_normalization() %>%
      layer_activation_relu()

    b <- a %>%
      layer_conv_1d(filters, kernel_size, padding = 'same') %>%
      layer_batch_normalization(renorm = renorm) %>%
      layer_activation_relu()

    c <- b %>%
      layer_conv_1d(filters, kernel_size, padding = 'same') %>%
      layer_batch_normalization(renorm = renorm)

    og_channels <- x$shape[[length(x$shape)+1L]] #%>% as.character %>%  as.integer

    residual <-
      if (downsample) {
        residual <- layer_conv_1d(x, og_channels, 1, padding = 'same')
      } else x

    out <- c %>% # will eventually be: se_module(c)
      layer_conv_1d(og_channels, 1, padding = 'same') %>%
      {layer_add(list(., residual))} %>%
      layer_activation_relu()

    out
  }



#' resblock with bottleneck from the Exploring Normalization FB paper (01/2017)
#' @export
resblock_batchnorm_bottle_2d <-
  function(x, filters = 64L, kernel_size = c(3L, 3L), downsample = TRUE) {
    a <- x %>%
      layer_conv_2d(filters, c(1L, 1L), padding = 'same') %>%
      layer_batch_normalization() %>%
      layer_activation_relu() %>%
      layer_conv_2d(filters, kernel_size, padding = 'same') %>%
      layer_batch_normalization() %>%
      layer_activation_relu()

    channels <- x$shape[-1L] %>% as.character %>%  as.integer

    output <- layer_conv_2d(a, channels, c(1L, 1L), padding = 'same') %>%
      layer_batch_normalization() %>%
      {layer_add(list(x, .))} %>%
      layer_activation_relu()

    output
  }


#' antirectifier resblock with bottleneck architecture
#' @export
antirect_resblock_1d <-
  function(x, filters = 32L, kernel_size = 3L, downsample = FALSE) {
    filters %<>% `%/%`(2L) %>% as.integer()
    conv1 <- layer_conv_1d(x, filters, kernel_size, padding = 'same')

    batchnorm <- conv1 %>%
      layer_batch_normalization() %>%
      layer_antirectifier_nd() %>%
      layer_conv_1d(filters, kernel_size, padding = 'same') %>%
      layer_batch_normalization()

    output <- batchnorm %>%
      {layer_add(list(., conv1))} %>%
      layer_antirectifier_nd()

    if(downsample)
      output %<>% layer_max_pooling_1d()

    output
  }


#' antirectifier resblock with bottleneck architecture
#' @export
antirect_resblock_2d <-
  function(x, filters = 32L, kernel_size = c(3L, 3L), downsample = FALSE) {
    filters %<>% `%/%`(2L) %>% as.integer()
    conv1 <- layer_conv_2d(x, filters, kernel_size, padding = 'same')

    batchnorm <- conv1 %>%
      layer_batch_normalization() %>%
      layer_antirectifier_nd(axis = 4) %>%
      layer_conv_2d(filters, kernel_size, padding = 'same') %>%
      layer_batch_normalization()

    output <- batchnorm %>%
      {layer_add(list(., conv1))} %>%
      layer_antirectifier_nd(axis = 4)

    if(downsample)
      output %<>% layer_max_pooling_2d(strides = c(2L, 1L),
                                       padding = 'same')

    output
  }
