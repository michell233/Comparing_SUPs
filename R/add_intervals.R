


if (FALSE) {  # Example 
  loc_tauargus <- "C:/TauArgus/TauArgus/TauArgus.exe"
  options(rtauargus.tauargus_exe = loc_tauargus)
  
  
  # All files in the R folder are sourced
  dir_R <- dir("R", full.names = TRUE)
  for(i in seq_along(dir_R)) {
    source(dir_R[i])  
  }
  
  hiers <- list(var1 = read_hier("nace_2"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  10000, n_unique = 1000)
  info_microdata(df, hiers)
  
  
  path <- "merged"
  dir.create(path)  # create if the folder does not exist
  
  filename <- "test2" 
  initialize_gauss(filename , df, hiers, path = path)
  add_sdcTable(filename, method = "SIMPLEHEURISTIC_OLD", path = path)
  add_sdcTable(filename, path = path)
  add_modular(filename, path = path)
  add_intervals(filename, multiple = FALSE)   # only intervals for one method 
  head(readRDS(file.path(path, paste0(filename, ".rds")))$df_merged, 10)
  add_intervals(filename, path = path)  # intervals for all methods not already computed
  head(readRDS(file.path(path, paste0(filename, ".rds")))$df_merged, 10)
}


# To add intervals results to the file
# 
# Intervals are computed for all methods that do not already have intervals, 
# and the file is updated with new results and saved after each method. 
# Use the multiple = FALSE if intervals should be computed for only a single method.
#
# Use output = "df_merged" to return a data frame instead (only one method)  
# 
add_intervals <- function(filename, 
                          path = "merged", 
                          output = NULL, 
                          multiple = TRUE,
                          lpPackage = "highs") {
  
  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  
  hrc_GAUSS <- all[["hierarchies"]]
  df_microdata <- all[["df_microdata"]]
  df_merged <- all[["df_merged"]]
  
  method_names <- sub("^suppressed_", "", grep("^suppressed_", names(df_merged), value = TRUE))
  interval_names <- sub("^lo_", "", grep("^lo_", names(df_merged), value = TRUE))
  compute_names <- setdiff(method_names, interval_names)
  

  if(!length(compute_names)) {
    stop(paste(method, "All intervals computed"))
  }
  
  method <- compute_names[1]
  
  
  i <- match(NA, df_merged$method)
  
  
  cat("\n\n\n")
  
  print(df_merged[seq_len(i-1), c("method", "elapsed", "error")])
  
  cat("\n\n\n  #####################################################################\n")
  cat("  #############   INTERVAL CALCULATIONS FOR   ################\n")
  cat("  #############   ", method, "      ################\n")
  cat("  #####################################################################\n")
  if( length(compute_names) > 1 ) {
    cat("  #  remaining methods:", paste(compute_names[-1], collapse = ", "), "\n\n\n")
  } else {
    cat("  ##############  last method         ###############\n\n\n")
  }
  
  
  primary <- df_merged[[paste("primary", method, sep = "_")]]
  suppressed <- df_merged[[paste("suppressed", method, sep = "_")]]
  forced <- !suppressed 
  hidden <- suppressed  & !primary 
  
  
  timing <- system.time({
    out  <- GaussSuppression::SuppressDominantCells(data=df_microdata,
                                  numVar = "response",
                                  hierarchies = hrc_GAUSS,
                                  preAggregate = TRUE,
                                  extraAggregate = FALSE,
                                  primary = primary,
                                  hidden = hidden, 
                                  forced = forced,
                                  singletonMethod = "none",
                                  lpPackage = lpPackage, 
                                  removeEmpty = TRUE,
                                  forcedInOutput = FALSE)
  })
  
  hier_names <- names(hrc_GAUSS)
  ok <- all.equal(out[hier_names], df_merged[hier_names])
  
  if (!isTRUE(ok)) {
    print(ok)
    stop("generated table was not identical")
  }
  
  df_merged$method[i] <-  paste0("interval_", method)
  df_merged$elapsed[i] <- unname(timing["elapsed"])
  
  df_merged[[paste0("lo_", method)]] <- out$lo
  df_merged[[paste0("up_", method)]] <- out$up
  
  if(identical(output,  "df_merged")){
    return(df_merged)
  }
  
  all[["df_merged"]] <- df_merged
  
  saveRDS(all, file.path(path, paste0(filename, ".rds")))
  
  if(multiple & length(compute_names) > 1) {
    add_intervals(filename, path = path, multiple = TRUE)
  } 
  invisible(NULL)
}
  




