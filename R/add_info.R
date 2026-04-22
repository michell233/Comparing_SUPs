

add_info <- function(df_merged, method, timing, try_result = NULL) {
  i <- match(NA, df_merged$method)
  df_merged$method[i] <- method
  df_merged$user[i] <- unname(timing["user.self"])
  df_merged$system[i] <- unname(timing["sys.self"])
  df_merged$elapsed[i] <- unname(timing["elapsed"])
  if (inherits(try_result, "try-error")) {
    error <- as.character(try_result)
    df_merged$error[i] <- error
  }
  df_merged
}