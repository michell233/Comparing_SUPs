###
#functions
check_real_hrc <- function(file) {
  txt <- readLines(file, warn = FALSE)
  any(grepl("@", txt, fixed = TRUE))
}

prepare_tauargus_inputs <- function(
    tab_gauss,
    hrc_GAUSS,
    hierarchies,
    loc_tauargus = "C:/TauArgus/TauArgus/TauArgus.exe"
) {
  
  ## 1. Input-Tabelle vorbereiten
  tab_modular_input <- tab_gauss %>%
    select(starts_with("var"), response, n_contr, primary) %>%
    mutate(no_pl = primary) %>%   # primaries ohne Schutzlevel
    filter(response > 0)
  
  ## 2. TauArgus-Pfad setzen
  options(rtauargus.tauargus_exe = loc_tauargus)
  
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
#Special functions are defined
#Using hidden limits interval calculation to the primary suppressed cells
primary_tau <- function(taudata, crossTable, ...) {
  ma <- SSBtools::Match(crossTable, taudata[names(crossTable)])
  if (!identical(range(diff(sort(ma))), c(1L, 1L)))
    stop("Matching failed")
  taudata$Status[ma] == 9
}
hidden_tau <- function(taudata, crossTable, ...) {
  ma <- SSBtools::Match(crossTable, taudata[names(crossTable)])
  if (!identical(range(diff(sort(ma))), c(1L, 1L)))
    stop("Matching failed")
  taudata$Status[ma] == 12
}
##
######run TauArgus with the same parameters
#prepare inputs for Argus
tau <- prepare_tauargus_inputs(tab_gauss, hrc_GAUSS, hierarchies)
#write all resulting data to global environment (For Debugging only)
list2env(tau, envir = .GlobalEnv)
#Measure Time
start_time_modular <- Sys.time()
#######Call Modular with rtauargus
ex1 <- tab_rtauargus(
  tab_modular_input,
  dir_name = "argus_files",
  files_name = "sy1",
  explanatory_vars = vars,
  hrc = hrc_vec,
  secret_var = "primary",
  secret_no_pl = "no_pl",
  value = "response",
  freq = "n_contr",
  totcode = totcode
)
end_time_modular <- Sys.time()
running_time_modular <- end_time_modular - start_time_modular

#######
#run "audit" on Modular result, i.e. calculate "up" and "lo" as in GAUSS solution
#change the status from "V","B"...to "2","9"and "12" for using GAUSS 
out_tau <- ex1 %>% 
  select(starts_with("var"),response,Status) %>% 
  mutate(Status = recode(Status,
                         "V" = 2,
                         "A" = 9,
                         "B" = 9,
                         "C" = 9,
                         "D" = 12))

#Run GAUSS to calculate "up" and "lo" as given by the TauArgus SUP
out_tau_2 <- SuppressDominantCells(data=df_microdata, 
                                   numVar = "response",
                                   hierarchies = hrc_GAUSS,
                                   contributorVar = "id",
                                   primary = primary_tau,
                                   hidden = hidden_tau,
                                   taudata = out_tau,
                                   lpPackage = "highs")
#Somehow some secondaries come back as NAs 
out_tau_2 <-  out_tau_2 %>% 
  mutate(suppressed = ifelse(is.na(suppressed),T,suppressed))
