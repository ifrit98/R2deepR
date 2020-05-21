


#' @export
phi <- 1.61803398874989484820458683436563811772

#' @export
e <- 2.718281828459045235360287471352662497757247093


#' Remove integer portion of a numeric and return only decimals
#' @export
return_decimal <- function(x) x %% 1 # or x - trunc(x)


#' Remove integer portion of numeric and return decimals.
#' Slightly faster than return_decimal.
#' @export
return_decimal2 <- function(x) x - trunc(x)


#' Split integer from decimal part of a numeric primitive.
#' Note: only the integer portion retains original sign(x)
#' @export
split_numeric <- function(x) {
  int <- trunc(x)
  dec <- x - int

  c(int, dec * sign(x))
}


#' Compute bth root of a
#' This is a so-called Vectorized function, i.e.
#' f(A,B) == c(f(A[1],B[1]), f(A[2],B[2]), ...) for vectors A, B
#' of equal length.
#'
#' This does *not* handle the case where b is the inverse of an integer
#' (e.g. nthroot(-1,1/3) ==? (-1)^3), since in general you cannot count
#' on 1/(1/int) being an integer.

#' If on the other hand you want the complex principal value of the nth
#' root, you can use (a+0i)^(1/b).
#' @export
nroot <- function(a, b) {
  if (b %% 2 == 1 | a >= 0) return(sign(a) * abs(a)^(1 / b))
  NaN
}



#' Generate fibonnaci sequence up to n
#' @export
fib <- function(n) {
  # stopifnot(is_scalar_integerish(n))
  n <- as.integer(n)
  f <- vector(mode = "list", length = n+1)
  f[[1]] <- 0
  f[[2]] <- 1
  for (i in 3:(n+1)) f[[i]] <- f[[i-1]] + f[[i-2]]

  f
}


#' Generate lucas numbers up to n
#' @export
lucas <- function(n) {
  # stopifnot(is_scalar_integerish(n))
  n <- as.integer(n)
  f <- vector(mode = "list", length = n+1)
  f[[1]] <- 2
  f[[2]] <- 1
  for (i in 3:(n+1)) f[[i]] <- f[[i-1]] + f[[i-2]]

  f
}



#' Generate prime numbers to a specified limit.
#'
#' Not particularly efficient, but gets the job done.
#' Try to keep it under 100000
#' @param limit Limit to generate up to.
#' @export
primes_n <- function(limit){
  n <- 2:limit
  i <- 1
  while(i < length(n)){
    p <- n[i]
    not.prime <- n[which(n %% p==0)]
    not.prime <- not.prime[! not.prime %in% p]
    n <- n[! n %in% not.prime]
    i <- i + 1
  }
  n
}


#' Compute n primes
#'
#' Try to keep it under 5000, or your CPU might explode
#'
#' Microbenchmark for n_primes (microseconds):
#'      expr             min         lq       mean
#' n_primes(10)       72.402       75.441   82.7702
#' n_primes(100)     2687.685     2852.065  2945.43
#' n_primes(1000)   417199.246   417258.025 426916
#' n_primes(2000)  1860778.072  1924292.325 1992891
#' n_primes(3000)  4366437.459  4403578.398 4413822
#' n_primes(5000) 13054581.632 13071920.879 13494860
#' @export
n_primes <- function(length.out) {
  n <- length.out
  i <- 3L
  count <- 2L
  primes <- vector("double", n)
  primes[1] <- 2

  while (count <= n) {

    for (c in 2:i) if (i %% c == 0) break

    if (c == i) {
      primes[count] <- i
      count <- count + 1L
    }
    i <- i + 1L
  }

  primes
}


#' Return both quotient and modulus
#' @export
mod <- function(a, b) c(quotient = floor(a / b), modulo = a %% b)



#' Random walk via sampling and looping
#' @export
rwalk <- function(n = 1000, values = c(-1, 1)) {
  x <- vector(mode = "numeric", length = n)

  for(i in 2:n) x[i] <- x[i - 1] + sample(values, 1)

  x
}


#' Random walk via cumsum
#' @export
rwalk2 <- function(n = 1000, values = c(-1, 1), seed = 17) {
  set.seed(seed)
  walk <- cumsum(sample(values, n, TRUE))
  walk
}


#' Compute binomial coefficient with a lookup table
#' @export
binom_coeff <- function(n, k) {
  C <- matrix(0, nrow = n + 1, ncol = k + 1)
  for (i in seq(n)+1) {
    for (j in seq(min(i, k) + 1)) {
      if (j == 1 | j == i)
        C[i, j] <- 1
      else
        C[i, j] <- C[i - 1, j - 1] + C[i - 1, j]
    }
  }
  C[n + 1, k + 1]
}


#' Generate pascal's triangle using a lookup table
#' @export
pascal <- function(x, return_all = FALSE) {
  tri      <- vector("list", x)
  tri[[1]] <- list(1)
  tri[[2]] <- list(1, 1)

  for (n in seq(3, x)) {
    row    <- vector("list", n)
    row[1] <- 1
    row[n] <- 1

    for (j in seq(2, length(row)-1))
      row[j] <- tri[[n - 1]][[j - 1]] + tri[[n - 1]][[j]]

    tri[[n]] <- row %>% unlist
  }
  if (return_all) tri else tri[[n]]
}



#' Farey sequence approximation algorithm to fractions, WIP
#' @export
as.fraction <- function(x, n = 1000, delta = 1e-2, return_precision = FALSE) {

  stopifnot(x <= 1)

  calc_match <- function(x, y) {
    xx    <- as.character(x)
    yy    <- as.character(y)
    match <- str_match(yy, paste0("^", xx))

    str_length(match[1]) - 2

  }

  return_helper <- function(x, y, frac) {
    frac  <- c(numerator = frac[1], denominator = frac[2])
    match <- calc_match(x, y)

    if (return_precision)
      stop("Unimplemented error... TBA")
    # return(list(
    #   fraction = frac,
    #   digits_matched = c(correct = match,
    #                      possible = str_length(y) - 2)
    # ))

    frac
  }

  is_close_enough <- function(x, y) {
    if (abs(y - x) <= delta) return(TRUE)
    FALSE
  }

  a <- 0
  b <- 1
  c <- 1
  d <- 1

  approx <- Inf
  best_a <- NULL
  best_b <- NULL
  best_c <- NULL
  best_d <- NULL

  while (b <= n && d <= n) {

    med <- (a + c) / (b + d)

    if (is_close_enough(x, med)) {
      approx <- med
      best_a <- a
      best_b <- b
      best_c <- c
      best_d <- d
    }

    if (x == med) {
      if (b + d <= n)
        return(return_helper((a + c) / (d + b), x, c(a + c, d + b)))
      else if (d > b)
        return(return_helper(c / d, x, c(c, d)))
      else
        return(return_helper(a / b, x, c(a, b)))
    }
    else if (x > med) {
      a <- a + c
      b <- b + d
    }
    else {
      c <- a + c
      d <- b + d
    }
  }

  if (b > n)
    return(return_helper(best_c / best_d, x, c(best_c, best_d)))
  else
    return(return_helper(best_a / best_b, x, c(best_a, best_b)))
}




#' Calculate all divisor pairs for a given number
#' @export
calculate_divisor_pairs <- function(x) {
  pairs <- list()
  d     <- get_divisors(x)
  n     <- 1

  for (i in d) {
    for (j in d) {
      y <- i * j
      if (y == x) {
        pairs[[n]] <- c(i, j)
        n <- n + 1
      }
    }
  }
  pairs
}


#' Get all divisors of a given number
#' @export
get_divisors <- function(x) {
  i   <- 1L
  j   <- 1L
  div <- c()
  while (i <= x) {
    if (x %% i == 0L) {
      div[j] <- i
      j <- j + 1L
    }
    i <- i + 1L
  }
  div
}


#' Greatest common divisor
#' @export
gcd <- function(x, y) {
  while(y) {
    temp = y
    y = x %% y
    x = temp
  }
  return(x)
}



#' Powers of 2 up to a given number.
#'
#' Takes log2 of n and calculates 2^x, where
#' x ranges from 1:log2(n)
#' @export
pow2_up_to <- function(n) {
  x <- floor(log2(n))
  powers_of_2(x)
}


#' List powers of 2 up to a given exponent, x
#' @export
powers_of_2 <- function(x) {
  x <- as.integer(x)
  l <- seq(1L, x)
  vapply(l, function(p) 2^p, 0)
}


#' Return powers of 2 within a specified range
#' @param min minimum of desired range
#' @param max maximum of desired range
#' @export
pow2_range <- function(min, max) {
  min    <- nearest_pow2(min)
  powers <- pow2_up_to(max(min, max))
  idx    <- which(powers == min)

  powers[idx:length(powers)]
}



#' Find nearest power of 2 to a numeric
#' @export
nearest_pow2 <- function(n) {
  fl <- floor(log2(n))
  cl <- ceiling(log2(n))

  pows <- powers_of_2(cl)
  lo <- pows[fl] %>% {abs(n - .)}
  hi <- pows[cl] %>% {abs(n - .)}

  if (lo > hi)
    return(pows[cl])
  else
    return(pows[fl])
}


