


read_hier <- function(hier_name, path = "data") {
  readRDS(paste0(path, "/", hier_name, ".rds"))
}


save_hier <- function(hier_name, path = "data") {
  hier <- get(hier_name)
  hier <- hier_as_df(hier)
  saveRDS(hier, paste0(path, "/", hier_name, ".rds"), compress = "xz")
}


# Hierarchy in standard form converted by sdcHierarchies
hier_as_df <- function(hier) {
  if (!is.data.frame(hier)) {
    hier <- SSBtools::Hrc2DimList(hier)
  }
  if (!inherits(hier, "sdc_hierarchy")) {
    hier <- hier_import(hier, from = "df")
  }
  sdcHierarchies::hier_convert(hier, as = "df")
}

prime_positions <- function(hiers) {
  a <- SSBtools::AutoHierarchies(hiers)
  for (i in seq_along(a)) {
    k <- unique(a[[i]]$mapsFrom)
    k <- k[!(k %in% a[[i]]$mapsTo)]
    a[[i]] <- k
  }
  a
}


#  "if(FALSE)" for code to not be run when sourcing the file 

if(FALSE) {
  
  # The code used to write hiers
  # Used with code from prepare_hrcs.R
  save_hier("region_large")
  save_hier("region_mid")
  save_hier("nace_5")
  save_hier("nace_3")
  save_hier("nace_2")
  large <- build_hierarchy(spec1)
  save_hier("large")
  table_header <- build_hierarchy(spec2)
  save_hier("table_header")
  minimal <- build_hierarchy(spec3)
  save_hier("minimal")
  
  # hier names from dir('data')
  hier_names <- gsub(".rds", "", dir("data"))
  
  
  list_hier_names <- as.list(hier_names)
  names(list_hier_names) <- hier_names
  
  
  # All hiers in a list read from data
  hiers <- lapply(list_hier_names, read_hier)
  
  
  # n codes
  sapply(hiers, nrow)
  
  # n prime positions
  sapply(prime_positions(hiers), length)
  
}