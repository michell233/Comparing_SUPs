


if (FALSE) {  # Example 
  hiers <- list(var1 = read_hier("minimal"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  100, n_unique = 20)
  initialize_gauss("test", df, hiers, overwrite = TRUE)
  all <- readRDS("merged/test.rds")
  head(all$df_merged)
}


# Function to initialize df_merged and to store a list with df_merged, df_microdata and hierarchies
# Use output = "all" to return the list 
# Use output = "df_merged" to return a data frame instead 
initialize_gauss <- function(filename, df_microdata, hierarchies, path = "merged", 
                             overwrite = FALSE,  
                             pvalue = 5, output = NULL) {
  
  
  for(nam in names(hierarchies)) {
    df_microdata[[nam]] <- toT(df_microdata[[nam]])
    hierarchies[[nam]][,2] <- toT(hierarchies[[nam]][,2]) 
  }
  
  remove_primary <- function(crossTable, ...) {
    rep(NA, nrow(crossTable))
  }
  
  cat(" initialize_gauss ...\n")
  flush.console()
  
  timing <- system.time({
    res <- GaussSuppression::SuppressDominantCells(
      data=df_microdata,
      hierarchies = hierarchies,
      numVar = "response", 
      pPercent = pvalue,
      allDominance = TRUE,
      singletonMethod = "none",
      primary = c(GaussSuppression::MagnitudeRule, remove_primary),
      protectionIntervals = TRUE, 
      intervalSuppression = FALSE,
      removeEmpty = TRUE)
  })
  
  res$pvalue <- 100*(1 - res$dominant2) / res$dominant1
  res$primary <- res$pvalue < pvalue
  
  remove_vars <- 
    c("dominant1", "dominant2", "max1contributor", "max2contributor", "n_non0_contr", "suppressed")
  
  res <- res[!(names(res) %in% remove_vars)]
  
  
  rename_vars <- names(res) %in% c("primary")
  names(res)[rename_vars] <- paste( names(res)[rename_vars],  "gauss", sep = "_")
  
  
  res$method <- NA
  res$elapsed <- NA
  res$error <- NA
  res$method[1] <-  "init_gauss_primary"
  res$elapsed[1] <- elapsed <- unname(timing["elapsed"])
  
  res <- add_mean_n_at(res, hierarchies)
  
  pp <- prime_positions(hierarchies)
  inner <- rep(TRUE, nrow(res))
  for (nam in names(pp)) {
    inner[!(res[[nam]] %in% pp[[nam]])] <- FALSE
  }
  res$inner <- inner
  
  if(identical(output,  "df_merged")){
    return(res)
  }
  
  all <- list(df_merged = res, df_microdata = df_microdata, hierarchies = hierarchies)
  
  if(identical(output,  "all")){
    return(all)
  }
  
  saveRDS2(all, file.path(path, paste0(filename, ".rds")), overwrite = overwrite)
  
}

saveRDS2 <- function(object, file, overwrite = FALSE, ...) {
  if (file.exists(file) && !overwrite) {
    stop("File already exists: ", file)
  }
  saveRDS(object, file = file, ...)
}


toT <- function(x) {
  x[toupper(x) == "TOTAL"] <- "T"
  x
}


