# To add analysis to the .rds file.
#
#  
# 

add_analysis <- function(filename, 
                      path = "merged", 
                      output = NULL) {
  #Load
  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  #initialise analysis list
  analysis <- list()
  
  
  result <- all$df_merged
  #add importance score for each cell
  result <- result |> 
    dplyr::mutate(importance_score = response / 10^(5 * mean_n_at))
  
  #identify secondary suppressed cells for each method:
  #GAUSS
  secondary_gauss <- result |>
    dplyr::filter(suppressed_gauss =="TRUE" & primary_gauss=="FALSE")
  #MODULAR
  secondary_modular <- result |>
    dplyr::filter(suppressed_modular=="TRUE" & primary_modular=="FALSE")
  #Simpleheuristic
  secondary_simple <- result |> 
    dplyr::filter(suppressed_simpleheuristic=="TRUE" & primary_simpleheuristic=="FALSE")
  #Simpleheuristic_old
  secondary_simple_old <- result |> 
    dplyr::filter(suppressed_simpleheuristic_old=="TRUE" & primary_simpleheuristic_old=="FALSE")
  #Number of secondaries per method
  secondaries_gauss <- nrow(secondary_gauss)
  secondaries_modular <- nrow(secondary_modular)
  secondaries_simple <- nrow(secondary_simple)
  secondaries_simple_old <- nrow(secondary_simple_old)
  #add to list
  analysis["no_secondaries"] <- list(c(secondaries_gauss,
                                                   secondaries_modular,
                                                   secondaries_simple,
                                                   secondaries_simple_old))
  
  #(absolute) suppressed response value for each method:
  value_surpressed_gauss <- sum(secondary_gauss$response)
  value_surpressed_modular <- sum(secondary_modular$response)
  value_surpressed_simple <- sum(secondary_simple$response)
  value_surpressed_simple_old <- sum(secondary_simple_old$response)
  #add to list
  analysis["absolute_value"] <- list(c(value_surpressed_gauss,
                                        value_surpressed_modular,
                                        value_surpressed_simple,
                                        value_surpressed_simple_old))
  #(relative to response of all cells) suppressed response value for each method:
  rel_value_surpressed_gauss <- sum(secondary_gauss$response)/sum(result$response)
  rel_value_surpressed_modular <- sum(secondary_modular$response)/sum(result$response)
  rel_value_surpressed_simple <- sum(secondary_simple$response)/sum(result$response)
  rel_value_surpressed_simple_old <- sum(secondary_simple_old$response)/sum(result$response)
  
  #add to list
  analysis["relative_value"] <- list(c(rel_value_surpressed_gauss,
                                       rel_value_surpressed_modular,
                                       rel_value_surpressed_simple,
                                       rel_value_surpressed_simple_old))
  
  #Hierarchical damage
  #suppressed importance relative to response of all cells
  importance_value_suppressed_gauss <- sum(secondary_gauss$importance_score)
  importance_value_suppressed_modular <- sum(secondary_modular$importance_score)
  importance_value_suppressed_simple <- sum(secondary_simple$importance_score)
  importance_value_suppressed_simple_old <- sum(secondary_simple_old$importance_score)
  
  analysis["importance_value_surpressed"]<- list(c(importance_value_suppressed_gauss,
                                                   importance_value_suppressed_modular,
                                                   importance_value_suppressed_simple,
                                                   importance_value_suppressed_simple_old))
  
  #Protectedness:
  #For how many primary suppressed cells [lo, up] is OK, i.e  
  #[lomax_response, upmin_respons] is covered by [lo, up]?
    
  #GAUSS
  Protectedness_gauss <- result |>
    dplyr::filter(lomax_response > lo_gauss & upmin_response < up_gauss)
  #in percent
  Protectedness_gauss_percent <- (length(Protectedness_gauss$primary_gauss)/ sum(result$primary_gauss == T)) *100
  #MODULAR
  Protectedness_modular <- result |>
    dplyr::filter(lomax_response > lo_modular & upmin_response < up_modular)
  #in percent
  Protectedness_modular_percent <- (length(Protectedness_modular$primary_modular)/ sum(result$primary_gauss == T)) *100
  #simple
  Protectedness_simple <- result |>
    dplyr::filter(lomax_response > lo_simpleheuristic & upmin_response < up_simpleheuristic)
  #in percent
  Protectedness_simple_percent <- (length(Protectedness_simple$primary_modular)/ sum(result$primary_gauss == T)) *100
  #simple
  Protectedness_simple_old <- result |>
    dplyr::filter(lomax_response > lo_simpleheuristic_old & upmin_response < up_simpleheuristic_old)
  #in percent
  Protectedness_simple_old_percent <- (length(Protectedness_simple_old$primary_modular)/ sum(result$primary_gauss == T)) *100
  
  #add to list
  analysis["Protectedness"] <- list(c(Protectedness_gauss_percent,
                                       Protectedness_modular_percent,
                                       Protectedness_simple_percent,
                                       Protectedness_simple_old_percent))
  
  #suppressions by "Class" to evaluate hierarchical damage
  #For GAUSS
  Class_gauss <- result |> 
    dplyr::filter(primary_gauss==F & suppressed_gauss == T) |> 
    dplyr::count(HiTaS_Class, name = "n_GAUSS")
  #For Modular
  Class_modular <- result|> 
    dplyr::filter(primary_modular ==F & suppressed_modular ==T) |> 
    dplyr::count(HiTaS_Class, name = "n_modular")
  #For Simple
  Class_simple <- result|> 
    dplyr::filter(primary_simpleheuristic ==F & suppressed_simpleheuristic ==T) |> 
    dplyr::count(HiTaS_Class, name = "n_simple")
  #For Simple Old
  Class_simple_old <- result|> 
    dplyr::filter(primary_simpleheuristic_old ==F & suppressed_simpleheuristic_old ==T) |> 
    dplyr::count(HiTaS_Class, name = "n_simple_old")
  #for comparing the two
  Classes_table <- dplyr::full_join(dplyr::full_join(dplyr::full_join(Class_gauss, Class_modular), Class_simple),Class_simple_old)
  
  #add to list
  analysis["Suppressions_by_Class"] <- list(Classes_table)
  
  #add to instance and save
  all[["analysis"]] <- analysis
  saveRDS(all, file.path(path, paste0(filename, ".rds")))
  
}
