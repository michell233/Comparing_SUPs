


if (FALSE) {  # Example 
  hiers <- list(var1 = read_hier("minimal"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  20, n_unique = 5)
  initialize_gauss("test", df, hiers)
}




# Function to initialize df_merged and to store a list with df_merged, df_microdata and hierarchies
initialize_gauss <- function(filename, df_microdata, hierarchies, path = "merged", overwrite = FALSE) {
  
  
  for(nam in names(hierarchies)) {
    df_microdata[[nam]] <- toT(df_microdata[[nam]])
    hierarchies[[nam]][,2] <- toT(hierarchies[[nam]][,2]) 
  }
  
  timing <- system.time({
    res <- GaussSuppression::SuppressDominantCells(
      data=df_microdata,
      hierarchies = hierarchies,
      numVar = "response", 
      contributorVar = "id",
      pPercent = pvalue,
      allDominance = TRUE,
      singletonMethod = "numttTtT",
      protectionIntervals = TRUE, 
      intervalSuppression = FALSE,
      removeEmpty = TRUE)
  })
  
  remove_vars <- 
    c("dominant1", "dominant2", "max1contributor", "max2contributor", "n_non0_contr")
  
  res <- res[!(names(res) %in% remove_vars)]
  
  method <- "gauss"
  
  rename_vars <- names(res) %in% c("primary", "suppressed")
  names(res)[rename_vars] <- paste( names(res)[rename_vars],  method, sep = "_")
  
  res$method <- NA
  res$elapsed <- NA
  res$error <- NA
  res$method[1] <-  method 
  res$elapsed[1] <- elapsed <- unname(timing["elapsed"])
  
  all <- list(df_merged = res, df_microdata = df_microdata, hierarchies = hierarchies)
  
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


