#' Creates a data-frame with differences between regression slopes of two
#' levels of a categorical variable, using simulation approach (see below). The
#' input data needs to have one categorical predictor variable, one continuous
#' predictor variable and one continuous response variable. The function fits a
#' linear regression model with lm(). It is your responsibility to explore the
#' data and check all linear regression assumptions before using  this function.
#' Any data transformation has to be done before using this function.
#' The simulation modeling is done as follows:
#' The continuous predictor values are sampled from the observed range of values,
#' with sample probabilities weighted using the density curve fitted on the
#' observed values. The two levels of the categorical predictor values are
#' sampled with probabilities equal to the relative frequencies of the levels
#' in the original data. The number of predictor values sampled is equal to the
#' number data points in the original data. Response variable data is obtained
#' by adding prediction errors to a slope of zero (type = simulate_0) or to the
#' predicted y-values (type = simulate_ci). Both the residuals and predicted
#' values are computed from the regression model that is fitted on the original
#' data. Output is a dataframe with two columns: replicate and stats. The latter
#' is the difference between the estimated regression slopes for the two levels
#' of the categorical predictor variable.
#' @mydata input data. A data frame with one categorical variable with two levels and two continuous variables
#' @continuous_variable continuous predictor variable
#' @categorical_variable categorical predictor variable. Maximum of two levels
#' @response_variable Numerical response variable
#' @type You want to test the null model of no difference between the slopes:
#' type = "null-hypothesis". You want to get the confidence interval around the
#' observed difference in regression slopes": "confidence-interval"
#' @reps number of replicates
#' @sample_size Number of data points generated for the simulated data sets. Defined
#' by a value that will be multiplied with the sample size of the original data.
#' Default is 1, which result in the same sample size as original data.
#' @equal_variance default is "yes". If "no", separate error terms are calculated
#' for each group
#' @error_term value is multiplied with the standard deviation of the residuals
#' of the regression model on the original data. This allows the user to assess
#' the effect of a lower or higher than noise than observed in the data. Default
#' value is 1, resulting in a error term equal to the sd of the observed residuals.
#' @import readr
#' @import purrr
#' @import tidyr
#' @import tidyselect
#' @import broom
#' @import rsample
#' @import dplyr
#' @export
simulate_slope_differences <- function(
  mydata = forest_succession,
  continuous_variable = "age",
  categorical_variable = "top_pos",
  response_variable = "sr",
  type="null-hypothesis",
  reps=1000,
  sample_size = 1,
  error_term = c(1, 2)
  equal_variance = TRUE){


  mydata <- mydata %>%
    dplyr::rename(x_cont = tidyselect::all_of(continuous_variable),
           x_cat = tidyselect::all_of(categorical_variable),
           y_var = tidyselect::all_of(response_variable)) %>%
    dplyr::select(x_cont, x_cat, y_var)

    # getting regression statistics of original data
    # Linear regression model
    regr_model <- lm(y_var ~ x_cont * x_cat, data = mydata)

    # Density distribution of the continuous x-values
    dens_distr_x <-density(mydata$x_cont,
                           n=max(1024,
                                 round(sample_size*nrow(mydata),0)))

    # Levels of the categorical x-value, with original proportions
    suppressMessages(
      cat_var <- mydata %>% group_by(x_cat) %>%
        dplyr::summarise(nr = n()) %>%
        ungroup()
    )


  if(equal_variance == TRUE){

    # get standard deviation of the residuals of original regression model
    suppressMessages(
      sd_original_residuals <- mydata %>%
      mutate(predy = predict(regr_model)) %>%
      dplyr::summarise(residuals = error_term*sd(predy-y_var)) %>%
      as.numeric()
    )

      if(type == "confidence-interval"){

        simdat = list()

        for(i in 1:reps){
          # sampling data (predicted values + sampling error)
          simdat[[i]] <-
            data.frame(x_cont = sample(dens_distr_x[[1]],
                                       round(sample_size*nrow(mydata),0),
                                       prob = dens_distr_x[[2]],
                                       replace=F),
                       x_cat = sample(cat_var$x_cat,
                                      size = round(sample_size*nrow(mydata),0),
                                      prob = cat_var$nr,
                                      replace = T)) %>%
            mutate(y_pop = predict(regr_model, newdata = .),
                   pred_error = rnorm(round(sample_size*nrow(mydata),0),
                                      0,
                                      sd_original_residuals),
                   y_var = y_pop + pred_error) %>%
            dplyr::select(x_cont, x_cat, y_var) %>%
            mutate(y_var = unname(y_var))
        }

        simdat <-  simdat %>% map(~ lm(y_var ~ x_cont * x_cat, data = .))

        tmp1 <- simdat %>%
          map_df(broom::tidy, .id = "id") %>%
          filter(grepl(":", term)) %>%
          dplyr::select(replicate = id,
                        slope_difference = estimate,
                        p_value = p.value) %>%
          mutate(replicate = as.integer(replicate))%>%
          na.omit()

        tmp1 <- simdat %>%
          map_df(broom::glance, .id = "id") %>%
          select(r2_adj = adj.r.squared) %>%
          bind_cols(tmp1,.)%>%
          na.omit()

        } else if(type == "null-hypothesis"){

        simdat = list()

        for(i in 1:reps){
          # sampling data (predicted values + sampling error)
          simdat[[i]] <-
            data.frame(x_cont = sample(dens_distr_x[[1]],
                                       size = round(sample_size*nrow(mydata),0),
                                       prob = dens_distr_x[[2]],
                                       replace=F),
                       x_cat = sample(cat_var$x_cat,
                                      size = round(sample_size*nrow(mydata),0),
                                      prob = cat_var$nr,
                                      replace = T)) %>%
            mutate(y_var = rnorm(round(sample_size*nrow(mydata),0),
                                 0,
                                 sd_original_residuals)) %>%
            mutate(y_var = unname(y_var))
        }

        simdat <-  simdat %>% map(~ lm(y_var ~ x_cont * x_cat, data = .))

        tmp1 <- simdat %>%
          map_df(broom::tidy, .id = "id") %>%
          filter(grepl(":", term)) %>%
          dplyr::select(replicate = id,
                        slope_difference = estimate,
                        p_value = p.value) %>%
          mutate(replicate = as.integer(replicate))%>%
          na.omit()

        tmp1 <- simdat %>%
          map_df(broom::glance, .id = "id") %>%
          select(r2_adj = adj.r.squared) %>%
          bind_cols(tmp1,.)%>%
          na.omit()

      } else {
        stop("set type at either 'null-hypothesis', or 'confidence-interval'")
      }

    } else if(equal_variance == FALSE){

        # getting regression statistics of original data
        # Linear regression model
        regr_model <- lm(y_var ~ x_cont * x_cat, data = mydata)

        # Density distribution of the continuous x-values
        dens_distr_x <-density(mydata$x_cont,
                               n=max(1024,
                                     round(sample_size*nrow(mydata),0)))

        # Levels of the categorical x-value, with original proportions
        suppressMessages(
          cat_var <- mydata %>% dplyr::group_by(x_cat) %>% dplyr::summarise(nr = n())
          )

        # get standard deviation of the residuals of original regression model
        # But calculated separately for each of the two groups
        suppressMessages(
          sd_original_residuals <- mydata %>%
          mutate(predy = predict(regr_model)) %>%
          group_by(x_cat) %>%
          dplyr::summarise(residuals = error_term*sd(predy-y_var))
          )


        if(type == "confidence-interval"){

          simdat = list()

          for(i in 1:reps){
            # sampling data (predicted values + sampling error)
            simdat[[i]] <-
              data.frame(x_cont = sample(dens_distr_x[[1]],
                                         round(sample_size*nrow(mydata),0),
                                         prob = dens_distr_x[[2]],
                                         replace=F),
                         x_cat = sample(cat_var$x_cat,
                                        size = round(sample_size*nrow(mydata),0),
                                        prob = cat_var$nr,
                                        replace = T)) %>%
              left_join(sd_original_residuals, by = "x_cat") %>%
              rowwise() %>%
              mutate(res   = error_term*rnorm(1, mean=0, sd=residuals)) %>%
              ungroup() %>%
              mutate(y_pop = predict(regr_model, newdata = .)) %>%
              mutate(y_var = y_pop + res) %>%
              #dplyr::select(x_cont, x_cat, y_var) %>%
              mutate(y_var = unname(y_var))
          }

          simdat <-  simdat %>% map(~ lm(y_var ~ x_cont * x_cat, data = .))

          tmp1 <- simdat %>%
            map_df(broom::tidy, .id = "id") %>%
            filter(grepl(":", term)) %>%
            dplyr::select(replicate = id,
                          slope_difference = estimate,
                          p_value = p.value) %>%
            mutate(replicate = as.integer(replicate))%>%
            na.omit()

          tmp1 <- simdat %>%
            map_df(broom::glance, .id = "id") %>%
            select(r2_adj = adj.r.squared) %>%
            bind_cols(tmp1,.)%>%
            na.omit()

        } else if(type == "null-hypothesis"){

          simdat = list()

          for(i in 1:reps){
            # sampling data (predicted values + sampling error)
            simdat[[i]] <-
              data.frame(x_cont = sample(dens_distr_x[[1]],
                                         size = round(sample_size*nrow(mydata),0),
                                         prob = dens_distr_x[[2]],
                                         replace=F),
                         x_cat = sample(cat_var$x_cat,
                                        size = round(sample_size*nrow(mydata),0),
                                        prob = cat_var$nr,
                                        replace = T)) %>%
              left_join(sd_original_residuals, by = "x_cat") %>%
              rowwise() %>%
              mutate(y_var = rnorm(1, 0, residuals)) %>%
              ungroup() %>%
              mutate(y_var = unname(y_var))
          }

          simdat <-  simdat %>% map(~ lm(y_var ~ x_cont * x_cat, data = .))

          tmp1 <- simdat %>%
            map_df(broom::tidy, .id = "id") %>%
            filter(grepl(":", term)) %>%
            dplyr::select(replicate = id,
                          slope_difference = estimate,
                          p_value = p.value) %>%
            mutate(replicate = as.integer(replicate))%>%
            na.omit()

          tmp1 <- simdat %>%
            map_df(broom::glance, .id = "id") %>%
            select(r2_adj = adj.r.squared) %>%
            bind_cols(tmp1,.)%>%
            na.omit()

          } else {
          stop("set type at either 'null-hypothesis', or 'confidence-interval'")
        }
    }

    attr(tmp1, "simulation_settings") <- list(continuous_variable = continuous_variable,
                                              categorical_variable = categorical_variable,
                                              response_variable = response_variable,
                                              type = type,
                                              reps = reps,
                                              sample_size = sample_size,
                                              error_term = error_term,
                                              equal_variance = equal_variance)

    attr(tmp1, "original_regr_model") <- list(tidy_output = broom::tidy(regr_model),
                                              glance_output = broom::glance(regr_model))

    attr(tmp1, "stat_value_for_comparison") <- broom::tidy(regr_model)[4,2]

    attr(tmp1, "statistics_to_be_tested") <- "difference in regression slopes"

    return(tmp1)
}


#devtools::install_github("datadrivenyale/climactor", build_vignettes = TRUE)










