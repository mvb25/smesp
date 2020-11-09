#' defines all the settings for your analysis and formats your data accordingly.
#' Currently, only two predictor variables are allowed in the case of a regression
#' analysis, and maximum one of the predictor variables can be a categorical
#' variable. The categorical variable can only have two levels.
#' @df dataframe with input data.
#' @test type of test. Options: regression slope, difference between two regression
#' slopes. More to come.
#' @cont_var_1 Assign a value to 'error_cont_1' or 'error_cont_1', depending whether
#' you used cont_var_1 or cont_var_2 to assign the continuous variable. A value
#' of 1 means that the error term is computed as the sd of the residuals of the
#' regression model on the original data. Values of 0.5 and 2 mean that the error
#' term is half or twice that, respectively, etc. This assumed homoscedasticity.
#' You can include Heteroscedasticity (not yet implemented!) by assigning a formula
#' that calculates the error term as function of the continuous predictor
#' variable"). Use this argument when only one continuous variable is used!
#' @cont_var_2 idem. A second continuous variable is only used when you want to
#' explore interaction effects.Not yet implemented!
#' @cat_var
#' @resp_var Numerical response variable
#' @error_cont_1
#' @error_cont_2
#' @error_cat Option (1): Assign one single number to the argument 'error_cat'.
#' This means you assume that there is no difference in the error term between
#' the two levels of the categorical variable. A value of 1 means that the error
#' term is computed as the sd of the residuals of the regression model on the
#' original data. Values of 0.5 and 2 mean that the error term is half or twice
#' that, respectively, etc. Option (2): Assign a vector with two named values,
#' one for each of the levels of the categorical variable. This assumes that you
#' assume that the error term differs between the two levels. Assigning two times
#' the same value results in the calculation of separate error terms - based on
#' the original data - per level of the categorical variable. If you assign two
#' different values, a single error term is calculated but each is multiplied by
#' the their corresponding value.
#' residuals.
#' @import readr
#' @import purrr
#' @import tidyr
#' @import tidyselect
#' @import dplyr
#' @export
simulate_slope_differences <- function(
  df               = forest_succession,
  test             = "regression slope",
  cont_var_1       = "age",
  cont_var_2       = NULL,
  cat_var          = "top_pos",
  resp_var         = "sr",
  error_cont_1     = 1,
  error_cont_2     = NULL,
  error_cat        = c(LS=1, US=2)){

  ###---------------------------------------------------------------------------
  if(test == "difference in regression slopes"){

    if(is.null(cat_var)){
      stop("Testing for the difference between two regression slopes requires
           one continuous and one categorical variable")
    }

    if(!is.null(cont_var_1) & !is.null(cont_var_2)){
      stop("This option requires one categorical and (only) one continuous
      variable. Use the argument 'cont_var_1' to define the latter")
    }

    if(is.null(cont_var_1) & !is.null(cont_var_2)){
      stop("Use the argument 'cont_var_1' to define your (only) continuous variable")
    }

    if(is.null(cat_var) & is.null(cont_var_1) & is.null(cont_var_2)){
      stop("Testing for the difference between two regression slopes requires
           one continuous and one categorical variable")
    }

    if(is.null(resp_var)){
      stop("You need a response variable")
    }

    nr_levels <- df %>% distinct(.[all_of(cat_var)]) %>% count() # FIGURE OUT HOW TO USE EXTRACTED VARIABLE AS COLUMN NAME IN DISTINCT()
    if(nr_levels != 2){
       stop("Currently this function only supports two levels for the categorical variable")
    }

    if(!is.numeric(df[,all_of(cont_var_1)]) & !is.numeric(df[,all_of(resp_var)])){
      stop("Both your response and predictor variables are not numeric")
    }

    if(!is.numeric(df[,all_of(cont_var_1)])){
      stop("your predictor variable is not numeric")
    }

    if(!is.numeric(df[,all_of(resp_var)])){
      stop("your response variable is not numeric")
    }

    if(is.null(error_cat)){
       stop("Fill in something! (Option 1): Assign one single number to the argument
       'error_cat'. This means you assume that there is no difference in the error
       term between the two levels of the categorical variabel. A value of 1 means
       that the error term is computed as the sd of the residuals of the regression
       model on the original data. Values of 0.5 and 2 mean that the error term is
       half or twice that, respectively, etc. (Option 2): Assign a vector with two
       named values, one for each of the levels of the categorical variable. This
       assumes that you assume that the error term differs between the two levels.
       Assigning two times the same value results in the calculation of seperate
       error terms - based on the original data - per level of the categorical
       variable. If you assign two different values, a single error term is
       calculated but each is multiplied by the their corresponding value.")
    }

     if(is.null(error_cont_1)  &  !is.null(cont_var_2) |
        !is.null(error_cont_1) &  !is.null(cont_var_2)){
       stop("Assign (only) a value to 'error_cont_1' to assign the error term of
       the continuous variable. A value of 1 means that the error term is computed
       as the sd of the residuals of the regression model on the original data.
       Values of 0.5 and 2 mean that the error term is half or twice that,
       respectively, etc. The default assumption is homoscedasticity. You can include
       heteroscadisticy (not yet implemented!) by instead of a number, assigning
       a formula that calculates the error term as function of the continuous
       predictor variable.")
     }

    if(length(error_cat) > 2){
      stop("Assign either one value or a vector with two values to the argument
      'error_cat'. (Option 1): Assign one single number to the argument
       'error_cat'. This means you assume that there is no difference in the error
       term between the two levels of the categorical variabel. A value of 1 means
       that the error term is computed as the sd of the residuals of the regression
       model on the original data. Values of 0.5 and 2 mean that the error term is
       half or twice that, respectively, etc. (Option 2): Assign a vector with two
       named values, one for each of the levels of the categorical variable. This
       assumes that you assume that the error term differs between the two levels.
       Assigning two times the same value results in the calculation of seperate
       error terms - based on the original data - per level of the categorical
       variable. If you assign two different values, a single error term is
       calculated but each is multiplied by the their corresponding value.")
    }

    df <- df %>%
          dplyr::select(tidyselect::all_of(cont_var_1),
                        tidyselect::all_of(cat_var),
                        tidyselect::all_of(resp_var))

    # Adding attributes to data frame
    variables <- list(
      attr(df, "response_variable")    <- resp_var,
      attr(df, "continuous_variable")  <- cont_var_1,
      attr(df, "categorical_variable") <- cat_var
    )

    if(lenght(error_cat) == 2){
        if(is.function(error_cont_1)){
          error_terms <- list(
           attr(df, "Heteroscedasticity_categorical") <- TRUE,
           attr(df, "Heteroscedasticity_continuous") <- TRUE,
           attr(df, "variance_structure_categorical") <- data.frame(error_cat) %>%
               rownames_to_column(all_of(cat_var)),
           attr(df, "variance_structure_continuous") <-  error_cont_1
        )
        } else {
          error_terms <- list(
            attr(df, "Heteroscedasticity_categorical") <- TRUE,
            attr(df, "Heteroscedasticity_continuous") <- FALSE,
            attr(df, "variance_structure_categorical") <- data.frame(error_cat) %>%
              rownames_to_column(all_of(cat_var)),
            attr(df, "variance_structure_continuous") <-  error_cont_1
          )
        }

      } else {
        if(is.function(error_cont_1)){
          error_terms <- list(
            attr(df, "Heteroscedasticity_categorical") <- FALSE,
            attr(df, "Heteroscedasticity_continuous") <- TRUE,
            attr(df, "variance_structure_categorical") <- data.frame(error_cat) %>%
              rownames_to_column(all_of(cat_var)),
            attr(df, "variance_structure_continuous") <-  error_cont_1
          )
        } else {
          error_terms <- list(
            attr(df, "Heteroscedasticity_categorical") <- FALSE,
            attr(df, "Heteroscedasticity_continuous") <- FALSE,
            attr(df, "variance_structure_categorical") <- data.frame(error_cat) %>%
              rownames_to_column(all_of(cat_var)),
            attr(df, "variance_structure_continuous") <-  error_cont_1
          )
        }
      }

  ###---------------------------------------------------------------------------
  } else if(test == "regression slope"){

    if(!is.null(cat_var)){
      stop("The option to test the regression slope requires only
           one continuous predictor and no categorical variable.
           Use the argument 'cont_var_1' to define the former")
    }

    if(!is.null(cont_var_1) & !is.null(cont_var_2)){
      stop("This option requires only one continuous predictor
      variable. Use the argument 'cont_var_1' to define this predictor variable")
    }

    if(is.null(cont_var_1) & !is.null(cont_var_2)){
      stop("Use the argument 'cont_var_1' to define your (only) continuous variable")
    }

    if(is.null(cat_var) & is.null(cont_var_1) & is.null(cont_var_2)){
      stop("The option to test the regression slope requires one continuous
           predictor. Use the argument 'cont_var_1' to define the former")
    }

    if(is.null(resp_var)){
      stop("You need a response variable")
    }

    if(!is.null(error_cat)){
      stop("The option to test the regression slope requires one continuous
           predictor. So there is no need to define the error term for a
           categorical variable")
    }

    if(!is.numeric(df[,all_of(cont_var_1)]) & !is.numeric(df[,all_of(resp_var)])){
      stop("Both your response and predictor variables are not numeric")
    }

    if(!is.numeric(df[,all_of(cont_var_1)])){
      stop("your predictor variable is not numeric")
    }

    if(!is.numeric(df[,all_of(resp_var)])){
      stop("your response variable is not numeric")
    }

    if(is.null(error_cont_1)  &  !is.null(cont_var_2) |
       !is.null(error_cont_1) &  !is.null(cont_var_2)){
      stop("Assign (only) a value to 'error_cont_1' to assign the error term of
       the continuous variable. A value of 1 means that the error term is computed
       as the sd of the residuals of the regression model on the original data.
       Values of 0.5 and 2 mean that the error term is half or twice that,
       respectively, etc. The default assumption is homoscedasticity. You can include
       heteroscadisticy (not yet implemented!) by instead of a number, assigning
       a formula that calculates the error term as function of the continuous
       predictor variable.")
    }

    df <- df %>%
      dplyr::select(tidyselect::all_of(cont_var_1),
                    tidyselect::all_of(resp_var))

    # Adding attributes to data frame
    variables <- list(
      attr(df, "response_variable")    <- resp_var,
      attr(df, "continuous_variable")  <- cont_var_1
    )

    if(is.function(error_cont_1)){
        error_terms <- list(
          attr(df, "Heteroscedasticity_continuous") <- TRUE,
          attr(df, "variance_structure_continuous") <-  error_cont_1
        )
      } else {
        error_terms <- list(
          attr(df, "Heteroscedasticity_continuous") <- FALSE,
          attr(df, "variance_structure_continuous") <-  error_cont_1
        )
      }

  } else { stop("at this moment the only option are 'regression slope' and 'difference in regression slopes'")}

    return(df)
}








