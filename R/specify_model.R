#' Specifies the model and sets the variables
#' Specify the type of analysis, the predictor and response variables, variance
#' structure, and whether to test a null hypothesis or generate a confidence
#' interval. Currently, only two predictor variables are allowed in the case of
#' a regression model and maximum one of the predictor variables can be a
#' categorical variable. The categorical variable can only have two levels.
#' @df dataframe with input data.
#' @test type of test. Options: regression slope, difference between regression
#' slopes. More to come.
#' @procedure choose whether you want to test a null-hypothesis (of a mean, regression
#' slope, slope difference or interaction effect of zero) or get a confidence
#' interval around the observed sample statistic.
#' @cont_var_1 Assign a value to 'error_cont_1' or 'error_cont_1', depending whether
#' you used cont_var_1 or cont_var_2 to assign the continuous variable. A value
#' of 1 means that the error term is computed as the sd of the residuals of the
#' regression model on the original data. Values of 0.5 and 2 mean that the error
#' term is half or twice that, respectively, etc. However, if you set the argument
#' 'heteroscedasticity' == TRUE, the error term will be calculated as function
#' of the continuous predictor variable. Use this argument when only one continuous
#' variable is used!
#' @cont_var_2 idem. A second continuous variable is only used when you want to
#' explore interaction effects.Not yet implemented!
#' @cat_var Categorical variable. All test options except for 'Chi-square' only
#' allow for two levels within the categorical variable.
#' @resp_var Numerical response variable
#' @error_cont_1 Multiplies the sd of the residuals of the regression model fitted
#' on the original data with given number. A value of '1' (default) means that
#' the error term of the model is the same as in the original data.
#' @heterosc_cont_1 A single number that defines how much smaller/bigger the
#' error term is at the highest value of the observed range of predictor values
#' than at the lowest value of the observed range of predictor values. The error
#' terms are then calculated such that the average of the error term over the
#' whole range of predictor values equals 'error_cont_1' * the sd of the residuals
#' of the regression model fitted on the original data.
#' @error_cont_2 see 'error_cont_1'
#' @error_cat A single number (when testing for difference between regression
#' slopes) or a vector with two numbers (testing for difference between the means)
#' that define how much smaller/bigger the error term than the standard deviation
#' of the residuals of the model fitted on the original data. A value of 1 means
#' that the sd of the residuals of the model fitted on the original data is used
#' to define the error term.
#' @heterosc_cont_2 see 'heterosc_cont_1'
#' @import readr
#' @import purrr
#' @import tidyr
#' @import tidyselect
#' @import dplyr
#' @export
specify_model <- function(
  df               = forest_succession,
  test             = "difference between regression slopes",
  procedure        = "null-hypothesis",
  cont_var_1       = "age",
  cont_var_2       = NULL,
  cat_var          = "top_pos",
  resp_var         = "sr",
  error_cat        = 1,
  error_cont_1     = 1,
  heterosc_cont_1  = 1,
  error_cont_2     = 1,
  heterosc_cont_2  = 1,
  heterosc_cat     = 1){

#---difference between regression slopes----------------------------------------
  if(test == "difference between regression slopes"){

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
       stop("Currently the option 'difference between regression slopes' only
            supports two levels for the categorical variable")
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

    df <- df %>%
          dplyr::select(tidyselect::all_of(cont_var_1),
                        tidyselect::all_of(cat_var),
                        tidyselect::all_of(resp_var))

    # Adding attributes to data frame
    test_procedure <- list(
      attr(df, "from")      <- "specify_model",
      attr(df, "test")      <- test,
      attr(df, "procedure") <- procedure
    )

    variables <- list(
      attr(df, "response_variable")    <- resp_var,
      attr(df, "continuous_predictor")  <- cont_var_1,
      attr(df, "categorical_predictor") <- cat_var
    )

    error_terms <- list(
        attr(df, "heterosc_cat")       <- heterosc_cat,
        attr(df, "error_cont_1")    <- error_cont_1,
        attr(df, "heterosc_cont_1") <- heterosc_cont_1)

    return(df)

# ---regression slope ----------------------------------------------------------
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


    if(!is.numeric(df[,all_of(cont_var_1)]) & !is.numeric(df[,all_of(resp_var)])){
      stop("Both your response and predictor variables are not numeric")
    }

    if(!is.numeric(df[,all_of(cont_var_1)])){
      stop("your predictor variable is not numeric")
    }

    if(!is.numeric(df[,all_of(resp_var)])){
      stop("your response variable is not numeric")
    }


    df <- df %>%
      dplyr::select(tidyselect::all_of(cont_var_1),
                    tidyselect::all_of(resp_var))

    # Adding attributes to data frame
    test_procedure <- list(
      attr(df, "from")      <- "specify_model",
      attr(df, "test")      <- test,
      attr(df, "procedure") <- procedure
    )

    variables <- list(
      attr(df, "response_variable")     <- resp_var,
      attr(df, "predictor_variable")  <- cont_var_1
    )

    error_terms <- list(
      attr(df, "error_cont_1")    <- error_cont_1,
      attr(df, "heterosc_cont_1") <- heterosc_cont_1
    )

    return(df)

#---difference between means---------------------------------------------
  } else if(test == "difference between means"){

    if(is.null(cat_var)){
      stop("Testing for the difference between two sample means requires
           one categorical variable")
    }

    if(!is.null(cont_var_1) | !is.null(cont_var_2)){
      stop("This option requires only one categorical and no continuous
      predictor variable(s). Use the argument 'cat_var' to define the former")
    }

    if(is.null(resp_var)){
      stop("You need a response variable")
    }

    nr_levels <- df %>% distinct(.[all_of(cat_var)]) %>% count()
    if(nr_levels != 2){
      stop("The option 'difference between sample means' only supports two levels
      for the categorical variable. Consider if the Chi-square test is an option")
    }

    if(!is.numeric(df[,all_of(resp_var)])){
      stop("your response variable is not numeric")
    }

    df <- df %>%
      dplyr::select(tidyselect::all_of(cat_var),
                    tidyselect::all_of(resp_var))

    # Adding attributes to data frame
    test_procedure <- list(
      attr(df, "from")      <- "specify_model",
      attr(df, "test")      <- test,
      attr(df, "procedure") <- procedure
    )

    variables <- list(
      attr(df, "response_variable")    <- resp_var,
      attr(df, "predictor_variable")   <- cat_var
    )

    error_terms <- list(
        attr(df, "error_cat") <- error_cat)

    return(df)

#---option does not exist--------------------------------------------------------

  } else { stop("at this moment the only option are 'difference between sample
                means', regression slope' and 'difference between regression
                slopes'")}

}








