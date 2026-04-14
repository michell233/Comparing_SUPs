


if (FALSE) {  # Example 
  hiers <- list(var1 = read_hier("minimal"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  20, n_unique = 5)
  df
  SSBtools::MakeFreq(df[1:2])
}


# Generates microdata in a way that can be manipulated in some ways
create_microdata <- function(hiers, n_ids =  1000, n_unique = n_ids/10, 
                             shape = 1.1, 
                             prob_dim = 1/seq_len(100000),
                             prob_freq = 1/seq_len(1000),
                             rnd_seed = 123,
                             sample_seed = 123) {
  
  if (!is.null(sample_seed)) {
    if (!exists(".Random.seed"))
      if (runif(1) < 0)
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(sample_seed )
  }
  pp <- prime_positions(hiers)
  dim_sizes <- sapply(pp, length)
  if (prod(dim_sizes) > 10^9) {
    stop("The data generation algorithm does not work with very many possible combinations.")
  }
  d <- skewsample(n_ids, n_unique, dim_sizes)
  for (i in seq_along(pp)) {
    d[[i]] <- pp[[i]][d[[i]]]
  }
  names(d) <- names(hiers)
  d$id <- seq_len(n_ids)
  add_response(d, shape = shape, rnd_seed = rnd_seed)
}



skewsample_unique <- function(n_unique, dim_sizes, prob_dim = 1/seq_len(1e+05)) {
  m_prob <- index_to_multi(seq_len(prod(dim_sizes)), dim_sizes)
  for (i in seq_along(dim_sizes)) {
    m_prob[, i] <- prob_dim[m_prob[, i]]
  }
  multi_index <- sample.int(prod(dim_sizes), size = n_unique, prob = apply(m_prob, 1, prod))
  as.data.frame(index_to_multi(multi_index, dim_sizes))
}


skewsample <- function(n, n_unique, dim_sizes, prob_dim = 1/seq_len(1e+05), prob_freq = 1/seq_len(1000)) {
  a <- skewsample_unique(n_unique, dim_sizes, prob_dim)
  a$freq <- sample_int(n_unique, n, prob_freq)
  a <- SSBtools::MakeMicro(a, "freq")
  a <- a[seq_along(dim_sizes)]
  a
}


sample_int <- function(size, tot, prob = 1/seq_len(1000)) {
  a_mean <- tot/size
  n <- n_from_mean(a_mean, prob)
  a <- 0
  while (sum(a) < tot) {
    a <- sample.int(n, size = size, replace = TRUE, prob = prob[seq_len(n)])
  }
  r <- runif(size)
  r <- r * (a - 1)
  r <- r * (sum(a) - tot)/sum(r)
  a <- a - r%/%1
  r <- r%%1
  ii <- order(r, decreasing = TRUE)[seq_len(sum(a) - tot)]
  a[ii] <- a[ii] - 1
  if (any(a <= 0)) {   # retry when 0 or negative
    return(sample_int(size, tot, prob))
  }
  a
}


n_from_mean <- function(a_mean, prob) {
  for (n in 1:1e+05) {
    if (sum(seq_len(n) * prob[seq_len(n)])/sum(prob[seq_len(n)]) >= a_mean) {
      return(n)
    }
  }
}


# written by ChatGPT
index_to_multi <- function(idx, dims) {
  if (any(idx < 1) || any(idx > prod(dims))) {
    stop("idx out of bounds")
  }
  
  idx0 <- idx - 1  
  
  res <- matrix(0L, nrow = length(idx), ncol = length(dims))
  
  for (k in seq_along(dims)) {
    res[, k] <- (idx0 %% dims[k]) + 1
    idx0 <- idx0 %/% dims[k]
  }
  
  colnames(res) <- paste0("dim", seq_along(dims))
  res
}

