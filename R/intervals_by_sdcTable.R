


if (FALSE) {  # Example 
  
  hiers <- list(var1 = read_hier("region_mid")[1:24, ],  
                var2 = read_hier("nace_2")[1:15, ],
                var3 = simple_hier(5, "A"))
  hiers <- lapply(hiers, remove_bogus)
  prod(sapply(hiers, nrow))
  prod(sapply(prime_positions(hiers), length))
  df <- create_microdata(hiers, n_ids = 5000, n_unique = 800, rnd_seed = 1)
  all <- check_primary(df, hiers)
  
  path <- "merged"
  dir.create(path)  # create if the folder does not exist
  filename <- "test4"
  initialize_gauss(filename, df, hiers, path = path, overwrite = TRUE)
  add_gauss(filename, path = path)
  
  system.time({
    add_intervals(filename, multiple = FALSE, path = path)
  })
  
  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  a <- all[["df_merged"]][c("lo_gauss", "up_gauss")]
  primary <- all[["df_merged"]]$primary_gauss
  
  system.time({
    b <- intervals_by_sdcTable(all[["hierarchies"]], 
                               all[["df_microdata"]], 
                               all[["df_merged"]],
                               primary_var = "primary_gauss",
                               suppressed_var = "suppressed_gauss")
  })
  
  max(abs(a[primary, ] - b[primary, ]))
  
  
}


# NOTE 1: This line in the code:
#   stop("Handling of bogus codes is not implemented")
# NOTE 2: "response" as  freqVar
intervals_by_sdcTable <- function(hierarchies,
                                  df_microdata,
                                  df_merged,
                                  primary_var = "primary",
                                  suppressed_var = "suppressed") {
  
  hier_names <- names(hierarchies)
  
  cat("\n", "[intervals_by_sdcTable.")
  flush.console()
  
  #create sdcProblem object
  obj <- sdcTable::makeProblem(
    data = df_microdata,
    dimList = hierarchies,
    freqVarInd = match("response", names(df_microdata)),  
    weightInd = NULL,
    sampWeightInd = NULL)
  
  
  cat(".")
  flush.console()
  
  sdcTable_attack(obj, 
                  df_external = df_merged, 
                  dim_var = hier_names, 
                  primary_var = primary_var,
                  suppressed_var =  suppressed_var)
  
}
  

# external_primary() extended with suppressed_var
# then sdcTable::attack
sdcTable_attack <- function(object, df_external, dim_var, primary_var = "primary",
                            suppressed_var = "suppressed") {
  
  # NOTE!!!!     ::: used here
  gdf_TRUE <- sdcTable:::g_df(object, addDups = TRUE, addNumVars = FALSE)
  cat(".")
  flush.console()
  gdf      <- sdcTable:::g_df(object, addDups = FALSE, addNumVars = FALSE)
  if (nrow(gdf_TRUE) != nrow(gdf)) {
    stop("Handling of bogus codes is not implemented")
  }
  rm(gdf_TRUE)
  
  cat(".")
  flush.console()
  
  gdf[, (dim_var) := NULL]
  data.table::setnames(gdf, old = paste0(dim_var, "_o"), new = dim_var)
  
  not_z <- which(gdf[["sdcStatus"]] != "z")
  
  if (data.table::is.data.table(df_external)) {
    df_external <- data.table::copy(df_external)
  } else {
    df_external <- data.table::as.data.table(df_external)
  }
  df_external[, row_id__ := .I]
  
  cat(".")
  flush.console()
  
  ma <- df_external[
    gdf[not_z],
    on = dim_var,
    x.row_id__
  ]
  
  if (anyNA(ma)) {
    stop("Matching failed: some rows from object have no match in df_external")
  }
  
  u_index <- not_z[df_external[[suppressed_var]][ma]]
  object <- sdcTable::setInfo(object, type = "sdcStatus", index = u_index, input = rep("x", length(u_index)))
  
  u_index <- not_z[df_external[[primary_var]][ma]]
  object <- sdcTable::setInfo(object, type = "sdcStatus", index = u_index, input = rep("u", length(u_index)))
  
  cat(".")
  flush.console()
  
  df_attack = as.data.frame(sdcTable::attack(object))
  
  cat(".")
  flush.console()
  
  df_attack = df_attack[match(u_index, df_attack$id), , drop = FALSE]
  
  df_intervals = data.frame(
    lo = rep(NA, nrow(df_external)),
    up = rep(NA, nrow(df_external))
  ) 
  
  r = ma[df_external[[primary_var]][ma]] 
  
  df_intervals$lo[r] = df_attack$low
  df_intervals$up[r] = df_attack$up
  
  cat("]\n")
  flush.console()
  
  df_intervals
}



