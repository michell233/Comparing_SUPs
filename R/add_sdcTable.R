


if (FALSE) {  # Example 
  hiers <- list(var1 = read_hier("nace_2"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  10000, n_unique = 1000)
  initialize_gauss("test1", df, hiers)
  add_sdcTable("test1")
  add_sdcTable("test1", method = "SIMPLEHEURISTIC_OLD")
  all <- readRDS("merged/test1.rds")
  head(all$df_merged)
}


# to add sdcTable results to the file
# Use parameter method to chose sdcTable method
# Use parameter output to return a data frame instead  
#      output = "out_simple" or output = "df_merged" 
add_sdcTable <- function(filename, path = "merged", output = NULL, method = "SIMPLEHEURISTIC", pvalue = 5) {
  
  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  
  hrc_GAUSS <- all[["hierarchies"]]
  df_microdata <- all[["df_microdata"]]
  df_merged <- all[["df_merged"]]

  if(method %in% df_merged$method) {
    stop(paste(method, "output already included"))
  }
  
  hier_names <- names(hrc_GAUSS)
  
  #create sdcProblem object
  prob.microDat <- sdcTable::makeProblem(
    data = df_microdata,
    dimList = hrc_GAUSS,
    freqVarInd = NULL,
    numVarInd = match("response", names(df_microdata)),
    weightInd = NULL,
    sampWeightInd = NULL)
  
  #primary suppressions
  prob.microDat <- sdcTable::primarySuppression(prob.microDat,type = "p", p=pvalue, numVarName="response")
  
  sdcTable_method <- method
  
  
  timing <- system.time({
    resSIMPLE <- try(sdcTable::protectTable(prob.microDat, method = sdcTable_method), silent = TRUE)
  })
  
  
  i <- match(NA, df_merged$method)
  
  df_merged$method[i] <-  method
  df_merged$elapsed[i] <- unname(timing["elapsed"])
  
  if (inherits(resSIMPLE, "try-error")) {
    ok <- FALSE
    error <- as.character(res)
    if(!is.null(output)){
      stop(error)
    } 
    df_merged$error[i] <- error
  } else {
    #output data.frame
    result_simpleheuristic <- sdcTable::getInfo(resSIMPLE, type = "finalData")
    
    result_simpleheuristic <- as.data.frame(result_simpleheuristic )
    
    out_simple <- result_simpleheuristic |> 
      dplyr::mutate(Status = dplyr::recode(sdcStatus,
                             "s" = 2,
                             "x" = 12,
                             "u" = 9)) |> 
      dplyr::select(-sdcStatus)
  
      
    out_simple <- as.data.frame(out_simple)
    
    # remove empty
    out_simple <- out_simple[out_simple$Freq!=0, , drop = FALSE]
    
    if(identical(output,  "out_simple")){
      return(out_simple)
    }
    
    primary_method <- paste("primary", tolower(method), sep = "_")
    suppressed_method <- paste("suppressed", tolower(method), sep = "_")
    
    
    df_merged[[primary_method]] <-  primary_tau(out_simple,  df_merged[hier_names])
    df_merged[[suppressed_method]] <- df_merged[[primary_method]] 
    df_merged[[suppressed_method]][hidden_tau(out_simple,  df_merged[hier_names])] <- TRUE
    
  }
  
  if(identical(output,  "df_merged")){
    return(df_merged)
  }
  
  all[["df_merged"]] <- df_merged
  
  
  saveRDS(all, file.path(path, paste0(filename, ".rds")))
  
  
}
  




