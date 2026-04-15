


if (FALSE) {  # Example 
  hiers <- list(var1 = read_hier("minimal"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  20, n_unique = 5)
  initialize("test", df, hiers)
}



# Function to initialize df_merged and to store a list with df_merged, df_microdata and hierarchies
initialize <- function(filename, df_microdata, hierarchies, path = "merged", overwrite = FALSE) {
  
  timing <- system.time({
    res <- GaussSuppressionFromData(data = df_microdata, 
                                    hierarchies = hierarchies, 
                                    numVar = "response", 
                                    primary = NULL, 
                                    protectZeros = FALSE, 
                                    removeEmpty = TRUE)
  })
  
  res <- res[!(names(res) %in% c("primary", "suppressed"))]
  
  res$method <- NA
  res$elapsed <- NA
  res$error <- NA
  res$method[1] <- "initial"
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

