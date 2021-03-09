#' Specifies the model and sets the variables
#' Specify the type of analysis, the predictor and response variables, variance
#' structure, and whether to test a null hypothesis or generate a confidence
#' interval. Currently, only two predictor variables are allowed in the case of
#' a regression model and maximum one of the predictor variables can be a
#' categorical variable. The categorical variable can only have two levels.
#' @df dataframe with input data.
#' @test type of test. Options: "slope" (regression slope), "diff slopes"
#' (difference between regression slopes), "diff intercept" (difference in
#' intercept with parallel slopes), "diff means" (different between two sample means
#' and "diff props" (difference between tow sample proportions. More to come.
#' @procedure choose whether you want to test a null-hypothesis ("H0", of a mean,
#' regression slope, or slope difference) or get a confidence interval around
#' the observed sample statistic ("CI").
#' @cont1 Assign a value to 'error_cont1' or 'error_cont2', depending whether
#' you used cont1 or cont2 to assign the continuous variable. A value
#' of 1 means that the error term is computed as the sd of the residuals of the
#' regression model on the original data. Values of 0.5 and 2 mean that the error
#' term is half or twice that, respectively, etc. However, if you set the argument
#' 'heteroscedasticity' == TRUE, the error term will be calculated as function
#' of the continuous predictor variable. Use this argument when only one continuous
#' variable is used!
#' @cont2 idem. A second continuous variable is only used when you want to
#' explore interaction effects.Not yet implemented!
#' @cat Categorical variable. All currently available test options only
#' allow for two levels within the categorical variable.
#' @resp Numerical response variable
#' @error_cont1 Multiplies the sd of the residuals of the regression model fitted
#' on the original data with given number. A value of '1' (default) means that
#' the error term of the model is the same as in the original data.
#' @het_cont1 A single number that defines how much smaller/bigger the
#' error term is at the highest value of the observed range of predictor values
#' than at the lowest value of the observed range of predictor values. The error
#' terms are then calculated such that the average of the error term over the
#' whole range of predictor values equals 'error_cont1' * the sd of the residuals
#' of the regression model fitted on the original data.
#' @error_cont2 see 'error_cont1'
#' @error_cat A vector with two numbers that defines how much
#' smaller/bigger the error term is than the standard deviation
#' of the residuals of the model fitted on the original data (both levels
#' combined). A value of 1 means that the sd of the residuals of the model fitted
#' on the original data (both levels combined) is used to define the error term.
#' If you want to use the sd of the original residuals calculated separately
#' for" each level, use "as data".
#' @het_cont2 see 'het_cont1'
#' @success in case of a test for difference in proportions, what level of the
#' categorical variable indicates success?
#' @import readr
#' @import purrr
#' @import tidyr
#' @import tidyselect
#' @import dplyr
#' @export
specify_model <- function(
  df,
  test,
  procedure,
  cont1       = NULL,
  cont2       = NULL,
  cat         = NULL,
  resp        = NULL,
  error_cat   = c(1,1),
  error_cont1 = 1,
  het_cont1   = 1,
  error_cont2 = 1,
  het_cont2   = 1,
  success){

#---difference between regression slopes----------------------------------------
  if(test == "diff slopes"){

    if(is.null(cat)){
      stop("Testing for the difference between two regression slopes requires
           one continuous and one categorical variable")
    }

    if(!is.null(cont1) & !is.null(cont2)){
      stop("This option requires one categorical and (only) one continuous
      variable. Use the argument 'cont1' to define the latter")
    }

    if(is.null(cont1) & !is.null(cont2)){
      stop("Use the argument 'cont1' to define your (only) continuous variable")
    }

    if(is.null(cat) & is.null(cont1) & is.null(cont2)){
      stop("Testing for the difference between two regression slopes requires
           one continuous and one categorical variable")
    }

    if(is.null(resp)){
      stop("You need a response variable")
    }

    nr_levels <- df %>% distinct(.[all_of(cat)]) %>% count() # FIGURE OUT HOW TO USE EXTRACTED VARIABLE AS COLUMN NAME IN DISTINCT()
    if(nr_levels != 2){
       stop("Currently the option 'diff slopes' only
            supports two levels for the categorical variable")
    }

    if(!is.numeric(data.frame(df)[,all_of(cont1)]) & !is.numeric(data.frame(df)[,all_of(resp)])){
      stop("Both your response and predictor variables are not numeric")
    }

    if(!is.numeric(data.frame(df)[,all_of(cont1)])){
      stop("your predictor variable is not numeric")
    }

    if(!is.numeric(data.frame(df)[,all_of(resp)])){
      stop("your response variable is not numeric")
    }

    df <- df %>%
          dplyr::select(tidyselect::all_of(cont1),
                        tidyselect::all_of(cat),
                        tidyselect::all_of(resp))

    # Adding attributes to data frame
    test_procedure <- list(
      attr(df, "from")      <- "specify_model",
      attr(df, "test")      <- test,
      attr(df, "procedure") <- procedure
    )

    variables <- list(
      attr(df, "response_variable")    <- resp,
      attr(df, "continuous_predictor")  <- cont1,
      attr(df, "categorical_predictor") <- cat
    )

    error_terms <- list(
        attr(df, "error_cat")      <- error_cat,
        attr(df, "error_cont1")    <- error_cont1,
        attr(df, "het_cont1")      <- het_cont1)

    return(df)


#---difference between intercepts----------------------------------------
   } else if(test == "diff intercepts"){

      if(is.null(cat)){
        stop("Testing for the difference between the intercepts of two
        regression lines requires one continuous and one categorical variable")
      }

      if(!is.null(cont1) & !is.null(cont2)){
        stop("This option requires one categorical and (only) one continuous
      variable. Use the argument 'cont1' to define the latter")
      }

      if(is.null(cont1) & !is.null(cont2)){
        stop("Use the argument 'cont1' to define your (only) continuous variable")
      }

      if(is.null(cat) & is.null(cont1) & is.null(cont2)){
        stop("Testing for the difference between the intercepts of two
        regression lines requires one continuous and one categorical variable")
      }

      if(is.null(resp)){
        stop("You need a response variable")
      }

      nr_levels <- df %>% distinct(.[all_of(cat)]) %>% count() # FIGURE OUT HOW TO USE EXTRACTED VARIABLE AS COLUMN NAME IN DISTINCT()
      if(nr_levels != 2){
        stop("Currently the option 'diff intercepts' only
            supports two levels for the categorical variable")
      }

      if(!is.numeric(data.frame(df)[,all_of(cont1)]) & !is.numeric(data.frame(df)[,all_of(resp)])){
        stop("Both your response and predictor variables are not numeric")
      }

      if(!is.numeric(data.frame(df)[,all_of(cont1)])){
        stop("your predictor variable is not numeric")
      }

      if(!is.numeric(data.frame(df)[,all_of(resp)])){
        stop("your response variable is not numeric")
      }

      df <- df %>%
        dplyr::select(tidyselect::all_of(cont1),
                      tidyselect::all_of(cat),
                      tidyselect::all_of(resp))

      # Adding attributes to data frame
      test_procedure <- list(
        attr(df, "from")      <- "specify_model",
        attr(df, "test")      <- test,
        attr(df, "procedure") <- procedure
      )

      variables <- list(
        attr(df, "response_variable")     <- resp,
        attr(df, "continuous_predictor")  <- cont1,
        attr(df, "categorical_predictor") <- cat
      )

      error_terms <- list(
        attr(df, "error_cat")      <- error_cat,
        attr(df, "error_cont1")    <- error_cont1,
        attr(df, "het_cont1")      <- het_cont1)

      return(df)


# ---regression slope ----------------------------------------------------------
  } else if(test == "slope"){

    if(!is.null(cat)){
      stop("The option to test the regression slope requires only
           one continuous predictor and no categorical variable.
           Use the argument 'cont1' to define the former")
    }

    if(!is.null(cont1) & !is.null(cont2)){
      stop("This option requires only one continuous predictor
      variable. Use the argument 'cont1' to define this predictor variable")
    }

    if(is.null(cont1) & !is.null(cont2)){
      stop("Use the argument 'cont1' to define your (only) continuous variable")
    }

    if(is.null(cat) & is.null(cont1) & is.null(cont2)){
      stop("The option to test the regression slope requires one continuous
           predictor. Use the argument 'cont1' to define the former")
    }

    if(is.null(resp)){
      stop("You need a response variable")
    }


    if(!is.numeric(data.frame(df)[,all_of(cont1)]) & !is.numeric(data.frame(df)[,all_of(resp)])){
      stop("Both your response and predictor variables are not numeric")
    }

    if(!is.numeric(data.frame(df)[,all_of(cont1)])){
      stop("your predictor variable is not numeric")
    }

    if(!is.numeric(data.frame(df)[,all_of(resp)])){
      stop("your response variable is not numeric")
    }


    df <- df %>%
      dplyr::select(tidyselect::all_of(cont1),
                    tidyselect::all_of(resp))

    # Adding attributes to data frame
    test_procedure <- list(
      attr(df, "from")      <- "specify_model",
      attr(df, "test")      <- test,
      attr(df, "procedure") <- procedure
    )

    variables <- list(
      attr(df, "response_variable")     <- resp,
      attr(df, "predictor_variable")  <- cont1
    )

    error_terms <- list(
      attr(df, "error_cont1")    <- error_cont1,
      attr(df, "het_cont1") <- het_cont1
    )

    return(df)

#---difference between means---------------------------------------------
  } else if(test == "diff means"){

    if(is.null(cat)){
      stop("Testing for the difference between two sample means requires
           one categorical variable")
    }

    if(!is.null(cont1) | !is.null(cont2)){
      stop("This option requires only one categorical and no continuous
      predictor variable(s). Use the argument 'cat' to define the former")
    }

    if(is.null(resp)){
      stop("You need a response variable")
    }

    nr_levels <- df %>% distinct(.[all_of(cat)]) %>% count()
    if(nr_levels != 2){
      stop("The option 'difference between sample means' only supports two levels
      for the categorical variable. Consider if the Chi-square test is an option")
    }
    # WEIRD! the original tibble gives a FALSE for this if statement!
    if(!is.numeric(data.frame(df)[,all_of(resp)])){
      stop("your response variable is not numeric")
    }

    df <- df %>%
      dplyr::select(tidyselect::all_of(cat),
                    tidyselect::all_of(resp))

    # Adding attributes to data frame
    test_procedure <- list(
      attr(df, "from")      <- "specify_model",
      attr(df, "test")      <- test,
      attr(df, "procedure") <- procedure
    )

    variables <- list(
      attr(df, "response_variable")    <- resp,
      attr(df, "predictor_variable")   <- cat
    )

    error_terms <- list(
        attr(df, "error_cat") <- error_cat)

    return(df)


#---difference between proportions---------------------------------------------
  } else if(test == "diff props"){

    if(is.null(cat)){
      stop("Testing for the difference between two sample proportions requires
           one categorical variable")
    }

    if(!is.null(cont1) | !is.null(cont2)){
      stop("This option requires only one categorical and no continuous
      predictor variable(s). Use the argument 'cat' to define the former")
    }

    if(is.null(resp)){
      stop("You need a response variable")
    }

    nr_levels <- df %>% distinct(.[all_of(cat)]) %>% count()
    if(nr_levels != 2){
      stop("The option 'difference between sample proportions' only supports two levels
      for the categorical variable. Consider if the Chi-square test is an option")
    }

    df <- df %>%
      dplyr::select(tidyselect::all_of(cat),
                    tidyselect::all_of(resp))

    # Adding attributes to data frame
    test_procedure <- list(
      attr(df, "from")      <- "specify_model",
      attr(df, "test")      <- test,
      attr(df, "procedure") <- procedure
    )

    variables <- list(
      attr(df, "response_variable")    <- resp,
      attr(df, "predictor_variable")   <- cat
    )

    proportion <- list(
      attr(df, "success") <- success)

    return(df)

#---option does not exist--------------------------------------------------------

  } else { stop("at this moment the only option are 'difference between sample
                means', regression slope' and 'difference between regression
                slopes'")}

}








