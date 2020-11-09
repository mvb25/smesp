#' defines all the settings for your analysis and formats your data accordingly.
#' Currently, only two predictor variables are allowed in the case of a regression
#' analysis, and maximum one of the predictor variables can be a categorical
#' variable. The categorical variable can only have two levels.
#' @df dataframe with input data.
#' @predictor_1 name and type in vector. When categorical, maximum of two levels.
#' @predictor_2 name and type in vector. When categorical, maximum of two levels.
#' @response Numerical response variable
#' @test type of test. Options: regression slope, difference between two regression
#' slopes, regression interaction effect, difference between means, and more to come.
#' @equal_variance default is TRUE. If FALSE, separate error terms are calculated
#' for each group
#' @var_struct The standard deviation of the residuals of the regression model
#' on the original data will be multiplied by this value. In case you want to create
#' unequal variance, provide a vector with two values, named after the two levels
#' of the categorical variable, or provide a function in case of you want to vary
#' the error term as function of the x-value. In case of equal variance, provide
#' just one number (default = 1). This allows to assess the effect of a lower or
#' higher than noise than observed in the data and to assess the effect of differences
#' in the error term between the two levels of a categorical variable. A value of
#' 1 results in error terms equal to the sd of the observed
#' residuals.
#' @import readr
#' @import purrr
#' @import tidyr
#' @import tidyselect
#' @import dplyr
#' @export
simulate_slope_differences <- function(
  df = forest_succession,
  test="difference in regression slopes",
  predictor_1 = c(pred = "age", type = "continuous"),
  predictor_2 = c(pred = "top_pos", type = "categorical"),
  response = "sr",
  var_struct = c(1, 2),
  equal_variance = TRUE){

  if(test = "difference in regression slopes"){

    if(length(var_struct) != 1 & equal_variance = TRUE){
          stop("if equal_variance = TRUE, provide just one value to var_struct")
        }

    if((predictor_1["type"] == "categorical" & predictor_2["type"] == "categorical") | (predictor_1["type"] == "numerical" & predictor_2["type"] == "numerical")){
      stop("Given your choice for the kind of test you want to run,
           include one continuous and one categorical variable")
    }

    if(is.null(predictor_1) | is.null(predictor_2)){
      stop("Given your choice for the kind of test you want to run,
           include one continuous and one categorical variable")
    }

    # FIGURE OUT HOW TO USE EXTRACTED VARIABLE AS COLUMN NAME IN DISTINCT()
    if(predictor_1["type"] == "categorical"){
       nr_levels <- df %>% distinct(.[predictor_1["pred"]]) %>% count()
       if(nr_levels > 2){
       stop("For now, a categorical variable can only have two levels")}
    }

    if(predictor_2["type"] == "categorical"){
      nr_levels <- df %>% distinct(.[predictor_2["pred"]]) %>% count()
      if(nr_levels > 2){
      stop("For now, a categorical variable can only have two levels")}
    }

    df <- df %>%
          dplyr::select(tidyselect::all_of(unname(predictor_1["pred"])),
                        tidyselect::all_of(unname(predictor_2["pred"])),
                        tidyselect::all_of(response))

    # Adding attributes
    attr(tmp1, "simulation_settings") <- list(continuous_variable = continuous_variable,
                                              categorical_variable = categorical_variable,
                                              response_variable = response_variable,
                                              type = type,
                                              reps = reps,
                                              sample_size = sample_size,
                                              error_term = error_term,
                                              equal_variance = equal_variance)

    } else { stop("at this moment the only option is 'difference in regression slopes'")}


    return(df)
}


#devtools::install_github("datadrivenyale/climactor", build_vignettes = TRUE)










