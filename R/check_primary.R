
  
if (FALSE) {  # Example 
  hiersA <- list(var1 = read_hier("nace_2"), var2 = read_hier("table_header"))
  dfA <- create_microdata(hiersA, n_ids = 10000, n_unique = 1000)
  
  allA <- check_primary(dfA, hiersA)
  names(allA)
  allA$info
  
  
  hiersB <- list(var1 = read_hier("minimal"), var2 = read_hier("table_header"))
  dfB <- create_microdata(hiersB, n_ids = 100, n_unique = 20)
  
  allB <- check_primary(dfB, hiersB, do_check_sdcTable = TRUE)
  names(allB)
  allB$ok_primary
  allB$info
  
  check_primary(dfA, hiersA, do_check_sdcTable = TRUE)$ok_primary
}


# Runs primary suppression and adds results from info_output(). 
# Use do_check_sdcTable = TRUE to also check that 
#       sdcTable::primarySuppression() gives the same result.
check_primary <- function(df_microdata, hierarchies, 
                          pvalue = 5, 
                          print_info = TRUE,
                          do_check_sdcTable = FALSE) {
  all <- initialize_gauss("nofile", df_microdata, hierarchies, output = "all")
  info <- info_output(all$df_merged, all$hierarchies)
  if (print_info) {
    print(info, quote = FALSE)
  }
  if(!do_check_sdcTable) {
    return(c(all, list(info = info)))
  }
  output_check <- check_sdcTable(all[[1]], all[[2]], all[[3]])
  all$df_merged$primary_sdcTable <- output_check$primary_sdcTable
  c(all, output_check, list(info = info))
}

# Generates information from the output. 
# Information on primary suppression is included, 
# but not secondary suppression. Similar to info_microdata().
info_output <- function(df_merged, hierarchies) {
  n_cells <- prod(sapply(hierarchies, nrow))
  n_empty <- n_cells - sum(df_merged$n_contr > 0)
  n_output <- nrow(df_merged)
  n_1 <- sum(df_merged$n_contr == 1)
  n_2 <- sum(df_merged$n_contr == 2)
  n_unsafe <- sum(df_merged$primary_gauss)
  inner_freq_max <- max(df_merged$n_contr[df_merged$inner])
  info <- c(n_cells = n_cells, n_empty = n_empty, n_output = n_output, n_1 = n_1, n_2 = n_2, n_unsafe = n_unsafe, inner_freq_max = inner_freq_max)
  info[2:6] <- paste0(info[2:6], " (", round(100 * info[2:6]/info[1]), "%)")
  # print(info, quote = FALSE)
  info
}


check_sdcTable <- function(df_merged, df_microdata, hierarchies, pvalue = 5) {
  
  
  #create sdcProblem object
  prob.microDat <- sdcTable::makeProblem(
    data = df_microdata,
    dimList = hierarchies,
    freqVarInd = NULL,
    numVarInd = match("response", names(df_microdata)),
    weightInd = NULL,
    sampWeightInd = NULL)
  
  
  obj <- sdcTable::primarySuppression(prob.microDat,type = "p", p=pvalue, numVarName="response")
  
  
  a <- as.data.frame(sdcTable::sdcProb2df(obj))

  hier_names <- names(hierarchies)
  
  a <- a[ , -match(hier_names, names(a))]  
  names(a)[match(paste0(hier_names, "_o"), names(a))] <- hier_names   
  
  output_sdcTable <- a
  
  # remove empty
  a <- a[a$sdcStatus!="z", , drop = FALSE]

  a$sdcStatus[a$sdcStatus=="u"] <- 9
  
  names(a)[names(a) == "sdcStatus"] = "Status"
  
  primary_sdcTable <- primary_tau(a,  df_merged[hier_names])
  
  ok_primary <- all.equal(primary_sdcTable, df_merged[["primary_gauss"]])
  
  if (!isTRUE(ok_primary)) {
    warning(paste("primary not as gauss:", ok_primary), call. = FALSE)
  }
  
  list(output_sdcTable = output_sdcTable,  
       primary_sdcTable = primary_sdcTable, 
       ok_primary = ok_primary)

}