


if (FALSE) {  # Example 
  hiers <- list(var1 = read_hier("minimal"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  100, n_unique = 20)
  initialize_gauss("test", df, hiers)
  add_modular("test")
}


# to add modular results to the file 
# Use parameter output to return a data frame instead  
#      output = "out_tau" or output = "df_merged" 
add_modular <- function(filename, path = "merged", output = NULL) {
  
  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  
  hrc_GAUSS <- all[["hierarchies"]]
  tab_gauss <-  all[["df_merged"]]
  
  if("modular" %in% tab_gauss$method) {
    stop("modular output already included")
  }

  clean_up()
  hrc_names <- paste0("hier",seq_along(hrc_GAUSS),"_df")
  
  for(i in seq_along(hiers)){
    write_argus_hrc(hiers[[i]], hrc_names[i])
  }

  #Read all hierarchy files
  hierarchies <- list.files(
    pattern = "^hier[0-9]+_df\\.hrc$",
    full.names = TRUE
  )
  hierarchies <- sub("^\\./", "", hierarchies)
  
  ######run TauArgus with the same parameters
  #prepare inputs for Argus
  tau <- prepare_tauargus_inputs(tab_gauss, hrc_GAUSS, hierarchies)
  
  #write all resulting data to global environment (For Debugging only)
  # list2env(tau, envir = .GlobalEnv)
  #Measure Time
  #start_time_modular <- Sys.time()
  #######Call Modular with rtauargus
  
  timing <- system.time({
    ex1 <- tab_rtauargus(
      tau$tab_modular_input,
      dir_name = "argus_files",
      files_name = "sy1",
      explanatory_vars = tau$vars,
      hrc = tau$hrc_vec,
      secret_var = "primary",
      secret_no_pl = "no_pl",
      value = "response",
      freq = "n_contr",
      totcode = tau$totcode
    )
  })

  
  out_tau <- ex1 %>% 
    select(starts_with("var"),response,Status) %>% 
    mutate(Status = recode(Status,
                           "V" = 2,
                           "A" = 9,
                           "B" = 9,
                           "C" = 9,
                           "D" = 12))
  
  if(identical(output,  "out_tau")){
    return(out_tau)
  }
  
  
  tab_gauss$primary_modular <-  primary_tau(out_tau,  tab_gauss[tau$vars])
  tab_gauss$suppressed_modular <- tab_gauss$primary_modular 
  tab_gauss$suppressed_modular[hidden_tau(out_tau,  tab_gauss[tau$vars])] <- TRUE
  tab_gauss
  
  
  i <- match(NA, tab_gauss$method)
  
  tab_gauss$method[i] <-  "modular"
  tab_gauss$elapsed[i] <- unname(timing["elapsed"])
  
  if(identical(output,  "df_merged")){
    return(tab_gauss)
  }
  
  all[["df_merged"]] <- tab_gauss
  
  saveRDS(all, file.path(path, paste0(filename, ".rds")))
  
}
  



write_argus_hrc <- function(df, hrc_name){
  write.table(
    df,
    file = paste0(hrc_name, ".hrc"),
    sep = "",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
  
  lines <- readLines(paste0(hrc_name, ".hrc"))
  lines <- sub("@@", "", lines)
  writeLines(lines[2:length(lines)], paste0(hrc_name, ".hrc"))
}


#clean working directory from previous run
clean_up <- function(x){
  hierarchies <- list.files(
    pattern = "^hier[0-9]+_df\\.hrc$",
    full.names = TRUE
  )
  if (length(hierarchies) > 0) {
    file.remove(hierarchies)
  }
  
  hierarchies_df <- list.files(
    pattern = "^hier[0-9]+_df",
    full.names = TRUE
  )
  if (length(hierarchies_df) > 0) {
    file.remove(hierarchies_df)
  }
}
###


###
#functions
check_real_hrc <- function(file) {
  txt <- readLines(file, warn = FALSE)
  any(grepl("@", txt, fixed = TRUE))
}

prepare_tauargus_inputs <- function(
    tab_gauss,
    hrc_GAUSS,
    hierarchies # loc_tauargus = "C:/TauArgus/TauArgus/TauArgus.exe"
) {
  
  # rename back to "primary" then continue with old code 
  tab_modular_input <- tab_gauss
  names(tab_modular_input)[names(tab_modular_input) == "primary_gauss"] <- "primary" 
  
  ## 1. Input-Tabelle vorbereiten
  tab_modular_input <- tab_modular_input  %>%
    select(starts_with("var"), response, n_contr, primary) %>%
    mutate(no_pl = primary) %>%   # primaries ohne Schutzlevel
    filter(response > 0)
  
  ## 2. TauArgus-Pfad setzen
  #options(rtauargus.tauargus_exe = loc_tauargus)
  
  ## 3. erklärende Variablen
  vars <- paste0("var", seq_along(hrc_GAUSS))
  
  ## 4. Hierarchien (named vector)
  hrc_vec <- setNames(
    hierarchies,
    paste0("var", seq_along(hierarchies))
  )
  
  ## 5. Reale vs. nicht-reale Hierarchien
  real_hrc <- sapply(hierarchies, check_real_hrc)
  
  ## 6. Nur reale Hierarchien an rtauargus übergeben
  hrc_vec <- hrc_vec[unname(real_hrc)]
  
  ## 7. Totcodes
  totcode <- setNames(
    rep("T", length(hierarchies)),
    paste0("var", seq_along(hierarchies))
  )
  
  list(
    tab_modular_input = tab_modular_input,
    vars = vars,
    hrc_vec = hrc_vec,
    totcode = totcode,
    real_hrc = real_hrc
  )
}







####  Functions written for other purpose, but reuse here  

### Special functions are defined
# Using hidden limits interval calculation to the primary suppressed cells
primary_tau <- function(taudata, crossTable, ...) {
  ma <- SSBtools::Match(crossTable, taudata[names(crossTable)])
  if (anyNA(ma) | !identical(range(diff(sort(ma))), c(1L, 1L)))
    stop("Matching failed")
  taudata$Status[ma] == 9
}
hidden_tau <- function(taudata, crossTable, ...) {
  ma <- SSBtools::Match(crossTable, taudata[names(crossTable)])
  if (anyNA(ma) | !identical(range(diff(sort(ma))), c(1L, 1L)))
    stop("Matching failed")
  taudata$Status[ma] == 12
}
forced_tau <- function(taudata, crossTable, ...) {
  ma <- SSBtools::Match(crossTable, taudata[names(crossTable)])
  if (anyNA(ma) | !identical(range(diff(sort(ma))), c(1L, 1L)))
    stop("Matching failed")
  !(taudata$Status[ma] %in% c(9, 12))
}







