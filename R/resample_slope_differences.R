#' Creates a data-frame with differences between regression slopes of two
#' levels of a categorical variable, using permuting or bootstrapping The input
#' data needs to have one categorical predictor variable, one continuous predictor
#' variable and one continuous response variable. The function fits a linear
#' regression model with lm(). It is your responsibility to explore the data and
#' check all linear regression assumptions before using this function. Any data
#' transformation has to be done before using this function. Output is a dataframe
#' with two columns: replicate and stats. The latter  is the difference between
#' the estimated regression slopes for the two levels of the categorical predictor
#' variable.
#' @mydata input data. A data frame with one categorical variable with two levels and two continuous variables
#' @continuous_variable continuous predictor variable
#' @categorical_variable categorical predictor variable. Maximum of two levels
#' @response_variable Numerical response variable
#' @type what kind of resampling procedure (bootstrap or permute) is used.
#' @repl number of replicates
#' @import dplyr
#' @import readr
#' @import purrr
#' @import tidyr
#' @import tidyselect
#' @import broom
#' @import rsample
#' @import dplyr
#' @export


resample_slope_differences <- function(
  mydata = forest_succession,
  continuous_variable = "age",
  categorical_variable = "top_pos",
  response_variable = "sr",
  type="permute",  # simulate with default original slope, but give option to change to whatever
  repl=1000){


  mydata <- mydata %>%
    rename(x_cont = tidyselect::all_of(continuous_variable),
           x_cat = tidyselect::all_of(categorical_variable),
           y_var = tidyselect::all_of(response_variable)) %>%
    dplyr::select(x_cont, x_cat, y_var)

  if(type == "permute") {

    tmp1 <- modelr::permute(mydata, repl, x_cat)$perm %>%
      map(~ lm(y_var ~ x_cont * x_cat, data = .)) %>%
      map_df(broom::tidy, .id = "id") %>%
      filter(grepl(":", term)) %>%
      dplyr::select(replicate = id, stat = estimate) %>%
      mutate(replicate = as.integer(replicate))

    return(tmp1)

  } else if(type == "bootstrap"){

    tmp1 <- bootstraps(mydata, times = repl)$splits %>%
      map(~ lm(y_var ~ x_cont * x_cat, data = .)) %>%
      map_df(broom::tidy, .id = "id") %>%
      filter(grepl(":", term)) %>%
      dplyr::select(replicate = id, stat = estimate) %>%
      mutate(replicate = as.integer(replicate)) %>%
      na.omit()

    return(tmp1)

  }else {
    stop("set type at either 'bootstrap' or 'permute'")
  }
}
