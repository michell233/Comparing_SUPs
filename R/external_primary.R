
if(FALSE) {
  df <- SSBtools::SSBtoolsData("sp_emp_withEU")
  hi <- SSBtools::FindDimLists(df[1:3])
  res <- GaussSuppression::SuppressDominantCells(
    df, 
    hierarchies  = hi, 
    dominanceVar = "value", 
    pPercent = 85)
  res
  
  obj <- sdcTable::makeProblem(
    data = df,
    dimList = hi,
    numVarInd = 5)
  
  obj1 <- external_primary(obj, res, names(hi))
  res1 <- sdcTable::protectTable(obj1, method = "SIMPLEHEURISTIC")
  final1 <- sdcTable::getInfo(res1, type = "finalData")
  final1 # same as res 
  
  
  obj2 <- sdcTable::primarySuppression(obj, type = "p", p = 85, numVarName = "value")
  res2 <- sdcTable::protectTable(obj2, method = "SIMPLEHEURISTIC")
  final2 <- sdcTable::getInfo(res2, type = "finalData")
  final2 # .. wrong ... old-Iceland should have been primary suppressed   
  
}

# External primary suppression for sdcTable
# Alternative to sdcTable::primarySuppression
external_primary <- function(object, df_external, dim_var, primary_var = "primary") {
  
  # NOTE!!!!     ::: used here 
  gdf <- sdcTable:::g_df(object, addDups = FALSE, addNumVars = FALSE)
  gdf[, (dim_var) := NULL]
  data.table::setnames(gdf, old = paste0(dim_var, "_o"), new = dim_var)
  
  not_z <- which(gdf[["sdcStatus"]] != "z")
  
  if (data.table::is.data.table(df_external)) {
    df_external <- data.table::copy(df_external)
  } else {
    df_external <- data.table::as.data.table(df_external)
  }
  df_external[, row_id__ := .I]
  
  ma <- df_external[
    gdf[not_z],
    on = dim_var,
    x.row_id__
  ]
  
  if (anyNA(ma)) {
    stop("Matching failed: some rows from object have no match in df_external")
  }
  
  u_index <- not_z[df_external[[primary_var]][ma]]
  
  sdcTable::setInfo(obj, type = "sdcStatus", index = u_index, input = rep("u", length(u_index)))
  
}
