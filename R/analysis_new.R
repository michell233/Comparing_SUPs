#For now static...need to make it flexible for all instances
result <- test$df_merged
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

#(absolute) suppressed response value for each method:
value_surpressed_gauss <- sum(secondary_gauss$response)
value_surpressed_modular <- sum(secondary_modular$response)
value_surpressed_simple <- sum(secondary_simple$response)
value_surpressed_simple_old <- sum(secondary_simple_old$response)
#(relative to response of all cells) suppressed response value for each method:
rel_value_surpressed_gauss <- sum(secondary_gauss$response)/sum(result$response)
rel_value_surpressed_modular <- sum(secondary_modular$response)/sum(result$response)
rel_value_surpressed_simple <- sum(secondary_simple$response)/sum(result$response)
rel_value_surpressed_simple_old <- sum(secondary_simple_old$response)/sum(result$response)

#Hierarchical damage
#suppressed importance relative to response of all cells
importance_value_surpressed_gauss <- sum(secondary_gauss$importance_score)
importance_value_surpressed_modular <- sum(secondary_modular$importance_score)
importance_value_surpressed_simple <- sum(secondary_simple$importance_score)
importance_value_surpressed_simple_old <- sum(secondary_simple_old$importance_score)

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


#implement also the count by "Classes" as defined in HiTaS approach. Maybe not needed for paper but
#interesting for practical comparison

#define Class by using "mean_n_at" column
#count dimensions
n_dims <- sum(startsWith(colnames(result),"var"))
#need to "calculate" by mapping the average to the group (bijection depends on
#number of dimensions)
if(n_dims == 2){
  result <-  result |>
    dplyr::mutate(HiTaS_Class = (mean_n_at*2)-2)
}else if(n_dims == 3){
result_test <-  result |>
  dplyr::mutate(HiTaS_Class = (mean_n_at*3)-3)
}else if(n_dims == 4){
  result_test <-  result |>
    dplyr::mutate(HiTaS_Class = (mean_n_at*4)-4)
}

#suppressions by "Class" to evaluate hierarchical damage
#For GAUSS
Class_gauss <- result |> 
  dplyr::filter(primary_gauss==F & suppressed_gauss == T) |> 
  dplyr::count(HiTaS_Class, name = "n_GAUSS")
#For Modular
Class_modular <- result|> 
  dplyr::filter(primary_modular ==F & suppressed_modular ==T) |> 
  dplyr::count(HiTaS_Class, name = "n_modular")
#for comparing the two
Classes_table <- dplyr::full_join(Class_gauss, Class_modular)
#calculate difference and add to table
Classes_table <- Classes_table |> 
  dplyr::mutate(delta= n_GAUSS - n_modular)



