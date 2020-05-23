#' Computes F1 score for a binary classifier given the logit and label vectors.
#'
#' @importFrom magrittr %<>%
f1_score_binary <- function(logits, labels) {

  # Unwrap tensors with $numpy() if `is_tensor()`
  if (is_tensor(logits) || is_tensor(labels)) {
    logits %<>% {.$numpy()}
    labels %<>% {.$numpy()}
  }

  stopifnot(dim(logits) == dim(labels))

  logits %<>% as.integer()
  labels %<>% as.integer()

  # Grab indices of all true class a values from labels array
  true_a <- which(labels == 1)

  true_pos  <- length(which(logits[true_a ] == 1))
  true_neg  <- length(which(logits[-true_a] == 0))
  false_pos <- length(which(logits[-true_a] == 1))
  false_neg <- length(which(logits[true_a ] == 0))

  # Compute precision (true pos / true pos + false pos)
  # Of examples recognized as cats, what % actually are cats
  p <- precision <- true_pos / (true_pos + false_pos)

  # Compute Recall (true pos / true pos + false neg)
  # What % of actual cats are correctly recorgnized?
  r <- recall <- true_pos / (true_pos + false_neg)

  # Compute F1 score
  f1 <- 2 / ((1/p) + (1/r))

  f1
}


