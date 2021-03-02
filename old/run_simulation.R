#' This function runs a simulation model multiple times and stores the statistics
#' of interest in a dataframe. The input data and model specifications are generated
#' by the function 'specify_simulation()'. In this function you define the
#' number pf replicates, the sample size in each simulation run, and the distribution
#' from which the x-values are randomly sampled.
#' @input_data input data. A data frame with one categorical variable with
#' two levels and two continuous variables
#' @reps number of replicates
#' @sample_size A number that gives the number of data points generated in the
#' simulation. The option 'as_data' returns the same number as in the original
#' data. The latter is default. In the case of 'difference between regression
#' slopes', the sample size per group will be equal or proportional to the sample
#' sizes in both groups. In the case of 'difference between sample_means', you can
#' set sample_size = "as_data" or provide a vector with two numbers, which allows
#' for setting different sample sizes per group.
#' @x_distr Two options: "uniform" or "as_data". These two options provide
#' two alternative probability distributions with which x-values are sampled
#' from the observed range of values (-/+ 5%). The option "uniform" speaks for
#' itself. Default option is "as_data". This option computes a density curve of the
#' x-values, which is used to provide the probabilities with which x-values
#' are sampled.
#' @import readr
#' @import purrr
#' @import tidyr
#' @import tidyselect
#' @import broom
#' @import rsample
#' @import dplyr
#' @export
run_simulation <- function(
  input_data    = mydf,
  reps          = 1000,
  sample_size   = "as_data",
  x_distr       = "as_data"){

#---regression slope------------------------------------------------------------
  if(attributes(input_data)$test == "regression slope"){

    # original data
    df <- input_data %>%
      rename(x_obs    = attributes(.)$predictor_variable,
             y_obs    = attributes(.)$response_variable) %>%
      mutate(y_pred   = predict(lm(y_obs ~ x_obs, data = .)),
             orig_res = y_pred-y_obs)

    # getting regression statistics of original data
    regr_model <- lm(y_obs ~ x_obs, data = df)

    # null hypothesis or confidence interval
    if(attributes(df)$procedure == "null-hypothesis"){
      use_this_intercept <- mean(df$y_obs)
      use_this_slope     <- 0
    } else {
      use_this_intercept <- coefficients(regr_model)[1]
      use_this_slope     <- coefficients(regr_model)[2]
    }

    sd_orig_res <- df %>% summarise(tmp = sd(orig_res)) %>% as.numeric()

    if(sample_size == "as_data"){nr_data_points = nrow(df)} else {nr_data_points = sample_size}

    simdat = list()

    for(i in 1:reps){

    # Getting predictor values and associated error terms
    if(x_distr == "uniform"){
      simdat[[i]] <- data.frame(sim_x    = seq(0.95*min(df$x_obs), 1.05*max(df$x_obs),
                                           length.out = max(1024, nr_data_points)),
                            heterosc = seq(1, attributes(df)$heterosc_cont_1,
                                           length.out = max(1024, nr_data_points))) %>%
        mutate(sdx = heterosc * (attributes(df)$error_cont_1 * sd_orig_res) / mean(heterosc)) %>%
        slice_sample(n=nr_data_points, replace=T) %>%
        mutate(y_pop    = use_this_intercept + use_this_slope * sim_x,
               sim_y = y_pop + rnorm(nr_data_points, 0, sdx)) %>%
        select(sim_x, sim_y)

    } else if(x_distr == "as_data") {
      # get coefficients of function of change in error term
      tmp <- data.frame(heterosc = seq(1, attributes(df)$heterosc_cont_1, length.out = nr_data_points)) %>%
        mutate(sim_x = seq(0.95*min(df$x_obs), 1.05*max(df$x_obs), length.out = nr_data_points),
               sdx   = heterosc * (attributes(df)$error_cont_1 * sd_orig_res) / mean(heterosc)) %>%
        lm(sdx ~ sim_x, data = .) %>%
        coefficients(.)

      # get x-value density distribution
      dens_distr_x <-density(df$x_obs,
                             n = max(1024, nr_data_points),
                             from = 0.95*min(df$x_obs),
                             to = 1.05*max(df$x_obs))

      # Get randomly selected x-values with probabilities given by the x-value density distribution
      simdat[[i]] <- data.frame(sim_x = sample(dens_distr_x[[1]], nr_data_points,prob = dens_distr_x[[2]], replace=T)) %>%
        mutate(sdx = tmp[1] + tmp[2]*sim_x) %>%
        mutate(y_pop    = use_this_intercept + use_this_slope * sim_x,
               sim_y = y_pop + rnorm(nr_data_points, 0, sdx)) %>%
        select(sim_x, sim_y)

    } else {stop("x_distr has to be 'as_data' or 'uniform")}
    }

    # Fitting a regression model on all data frames
    simdat <-  simdat %>% map(~ lm(sim_y ~ sim_x, data = .))

    # Extracting regression statistics
    tmp1 <- simdat %>%
      map_df(broom::tidy, .id = "id") %>%
      filter(term == "(Intercept)") %>%
      dplyr::select(replicate = id,
                    intercept = estimate,
                    p_intercept = p.value) %>%
      mutate(replicate = as.integer(replicate))%>%
      na.omit()

    tmp1 <- simdat %>%
      map_df(broom::tidy, .id = "id") %>%
      filter(term == "sim_x") %>%
      dplyr::select(replicate = id,
                    regr_slope = estimate,
                    p_slope = p.value) %>%
      mutate(replicate = as.integer(replicate))%>%
      left_join(tmp1,., by = c("replicate"))%>%
      na.omit()

    tmp1 <- simdat %>%
      map_df(broom::glance, .id = "id") %>%
      select(r2_adj = adj.r.squared) %>%
      bind_cols(tmp1,.)%>%
      na.omit()

    attr(tmp1, "model_specifications") <- attributes(input_data)

    attr(tmp1, "resampling_specifications") <- list(reps        = reps,
                                                    x_distr     = x_distr,
                                                    sample_size = sample_size)

    attr(tmp1, "regr_model_original_data") <- list(tidy_output = broom::tidy(regr_model),
                                              glance_output = broom::glance(regr_model))

    attr(tmp1, "test_stat") <- unname(coefficients(regr_model)[2])

    return(tmp1)


#---difference between regression slopes----------------------------------------
  } else if(attributes(input_data)$test == "difference between regression slopes"){

    # original data
    df <- input_data %>%
      rename(x_cont   = attributes(.)$continuous_predictor,
             x_cat    = attributes(.)$categorical_predictor,
             y_obs    = attributes(.)$response_variable) %>%
      mutate(y_pred   = predict(lm(y_obs ~ x_cont*x_cat, data = .)),
             orig_res = y_pred-y_obs)

    # groups
    group_data <- df %>% mutate(overall_sd = sd(orig_res)) %>%
      group_by(x_cat) %>%
      summarise(nr_original = n(),
                overall_sd = mean(overall_sd)) %>%
      ungroup() %>%
      # calculates the error terms for the two groups given provided ratio
      # group1/group2 while keeping the mean of the two error terms equal to the
      # overall error term
      mutate(grp_err = case_when(row_number() == 1 ~ attributes(df)$heterosc_cat * (2*overall_sd)/(attributes(df)$heterosc_cat+1),
                                 row_number() == 2 ~ (2*overall_sd)/(attributes(df)$heterosc_cat+1))) %>%
      # Getting proportion of observations (data points) per group
      mutate(prop_obs = nr_original / sum(nr_original)) %>%
      # define number of observations (= # of data points) per group
      # Use ifelse because with case_when all RHSs must evaluate to the same type of vector.
      # and when sample_size = "as_data", that is not possible
      mutate(nr_new = ifelse(sample_size == "as_data", nr_original, prop_obs*sample_size))


    ### Getting simulated x and y values

    simdat = list()

    for(i in 1:reps){

    if(x_distr == "uniform"){

      # Categorical variable, level 1
      grp1_range <- df %>%
        filter(x_cat == group_data$x_cat[1]) %>%
        summarise(min = 0.95*min(x_cont), max = 1.05*max(x_cont))

      grp1 <- data.frame(x_cont = seq(grp1_range$min, grp1_range$max,
                                      length.out = max(1024, group_data$nr_new[1])),
                         heterosc = seq(1, attributes(df)$heterosc_cont_1,
                                        length.out = max(1024, group_data$nr_new[1]))) %>%
        mutate(sdx = heterosc * (attributes(df)$error_cont_1 * group_data$grp_err[1]) / mean(heterosc)) %>%
        slice_sample(n = group_data$nr_new[1], replace=T) %>% select(-heterosc) %>%
        mutate(x_cat = group_data$x_cat[1])

      # Categorical variable, level 2
      grp2_range <- df %>%
        filter(x_cat == group_data$x_cat[2]) %>%
        summarise(min = 0.95*min(x_cont), max = 1.05*max(x_cont))

      grp2 <- data.frame(x_cont = seq(grp2_range$min, grp2_range$max,
                                      length.out = max(1024, group_data$nr_new[2])),
                         heterosc = seq(1, attributes(df)$heterosc_cont_1,
                                        length.out = max(1024, group_data$nr_new[2]))) %>%
        mutate(sdx = heterosc * (attributes(df)$error_cont_1 * group_data$grp_err[2]) / mean(heterosc)) %>%
        slice_sample(n = group_data$nr_new[2], replace=T) %>% select(-heterosc) %>%
        mutate(x_cat = group_data$x_cat[2])

      # Categorical variable, join two levels
      xy_data <- bind_rows(grp1, grp2)


    } else if(x_distr == "as_data") {
      # Categorical variable, level 1
      # figure out how to use map() here!
      x_dens_distr <- df %>%
        filter(x_cat == group_data$x_cat[1])

      x_dens_distr = density(x_dens_distr$x_cont,
                             n = max(1024, group_data$nr_new[1]),
                             from = 0.95*min(x_dens_distr$x_cont), to = 1.05*max(x_dens_distr$x_cont))

      grp1 <- data.frame(x_cont = x_dens_distr$x,
                         heterosc = seq(1, attributes(df)$heterosc_cont_1,
                                        length.out = max(1024, group_data$nr_new[1]))) %>%
        mutate(sdx = heterosc * (attributes(df)$error_cont_1 * group_data$grp_err[1]) / mean(heterosc)) %>%
        slice_sample(n = group_data$nr_new[1], weight_by = x_dens_distr$y, replace=T) %>% select(-heterosc) %>%
        mutate(x_cat = group_data$x_cat[1])

      # Categorical variable, level 2
      x_dens_distr <- df %>%
        filter(x_cat == group_data$x_cat[2])

      x_dens_distr = density(x_dens_distr$x_cont,
                             n = max(1024, group_data$nr_new[2]),
                             from = 0.95*min(x_dens_distr$x_cont), to = 1.05*max(x_dens_distr$x_cont))

      grp2 <- data.frame(x_cont = x_dens_distr$x,
                         heterosc = seq(1, attributes(df)$heterosc_cont_1,
                                        length.out = max(1024, group_data$nr_new[2]))) %>%
        mutate(sdx = heterosc * (attributes(df)$error_cont_1 * group_data$grp_err[2]) / mean(heterosc)) %>%
        slice_sample(n = group_data$nr_new[2], weight_by = x_dens_distr$y, replace=T) %>% select(-heterosc) %>%
        mutate(x_cat = group_data$x_cat[2])

      # Categorical variable, join two levels
      xy_data <- bind_rows(grp1, grp2)

    }

      # When null hypothesis: regression statistics of the model fitted on original
      # data without the categorical variable (i.e., no difference in intercept and slope)
      if(attributes(df)$procedure == "null-hypothesis"){
        lm.tmp <- lm(y_obs ~ x_cont, data = df)
      } else {
        # When confidence interval: regression statistics of the model fitted on
        # original data with the categorical variable
        lm.tmp  <- lm(y_obs ~ x_cat*x_cont, data = df)}

        simdat[[i]] <- xy_data %>%
          mutate(y_pop =  predict(lm.tmp, newdata = xy_data)) %>%
          rowwise() %>%
          mutate(sim_y = y_pop + rnorm(1, 0, sdx)) %>%
          ungroup() %>%
          select(x_cont, x_cat, sim_y)
    }
      # fitting a regression model on all data frames
      simdat <-  simdat %>% map(~ lm(sim_y ~ x_cat*x_cont, data = .))

      # Extracting regression statistics
      tmp1 <- simdat %>%
        map_df(broom::tidy, .id = "id") %>%
        filter(grepl(":", term)) %>%
        dplyr::select(replicate = id,
                      interaction = estimate,
                      p_value = p.value) %>%
        mutate(replicate = as.integer(replicate))%>%
        na.omit()

      # Extracting R2 statistics
      tmp1 <- simdat %>%
        map_df(broom::glance, .id = "id") %>%
        select(r2_adj = adj.r.squared) %>%
        bind_cols(tmp1,.)%>%
        na.omit()


      attr(tmp1, "model_specifications") <- attributes(input_data)

      attr(tmp1, "resampling_specifications") <- list(reps        = reps,
                                                      x_distr     = x_distr,
                                                      sample_size = sample_size)

      regr_model <- lm(y_obs ~ x_cat*x_cont, data = df)

      attr(tmp1, "test_stat") <- unname(coefficients(regr_model)[4])

      attr(tmp1, "regr_model_original_data") <- list(tidy_output = broom::tidy(regr_model),
                                                     glance_output = broom::glance(regr_model))

      return(tmp1)

#---difference between means---------------------------------------------
  } else if(attributes(input_data)$test == "difference between means"){

    # original data
    df <- input_data %>%
      rename(x_obs    = attributes(.)$predictor_variable,
             y_obs    = attributes(.)$response_variable)

    # extract the error_cat values from the attributes of the original data
    if(length(attributes(df)$error_cat) == 1){
      mf <- c(attributes(df)$error_cat, attributes(df)$error_cat)
    } else{
      mf <- c(attributes(df)$error_cat)
    }

    # get the means of the levels of the categorical variable and the difference
    # between these means
    model_stats <- df %>%
      group_by(x_obs) %>%
      summarise(means = mean(y_obs)) %>%
      ungroup() %>%
      pivot_wider(names_from = x_obs, values_from = means) %>%
      mutate(diff_means= .[[1,1]]-.[[1,2]])

    # Get the overall mean plus the mean of the residuals (difference between
    # group mean and observed values).
    model_stats <- df %>%
      group_by(x_obs) %>%
      # the residuals are calculated as the difference between observed y and
      # the group mean of the observed Y
      mutate(residual  = y_obs - mean(y_obs)) %>%
      # the following two lines mean that we calculate a single sd of the errors
      # (observed - mean), i.e., we assume the same error term across groups. That
      # might not be true (it is not true for the example data!). If you want to
      # use the observed difference in error term, use 'heterosc_cat', based on
      # values calculated prior to using this function.
      ungroup() %>%
      mutate(sd_resid = sd(residual)) %>%
      summarise(mean_y_obs = mean(y_obs),
                sd_resid = mean(sd_resid)) %>%
      # multiply the error term with user-provided factor
      bind_cols(model_stats, .)

    # Define the size of the sample
    if(length(sample_size) == 1){
      if(sample_size == "as_data"){
        nr_samples <- df %>% group_by(x_obs) %>% summarise(nr = n())
      } else {
        nr_samples <- data.frame(nr = c(sample_size, sample_size))
      }
    } else if(length(sample_size) == 2){
      nr_samples <- data.frame(nr = sample_size)
    }

    outcome <- data.frame(t_value = numeric(reps), p_value = numeric(reps), diff_means = numeric(reps))

    for(i in 1:reps){
    # Get a random sample
    if(attributes(df)$procedure == "confidence interval"){
      # sample error term and add group means
      grp1 = purrr::modify(rnorm(nr_samples$nr[1],
                                 0, mf[1]*model_stats$sd_resid),
                           ~ .x + model_stats[[1]])
      grp2 = purrr::modify(rnorm(nr_samples$nr[2],
                                 0, mf[2]*model_stats$sd_resid),
                           ~ .x + model_stats[[2]])

      } else if(attributes(df)$procedure == "null-hypothesis"){

      # sample error term and add mean_y_obs
      grp1 = purrr::modify(rnorm(nr_samples$nr[1],
                                 0, mf[1]*model_stats$sd_resid),
                           ~ .x + model_stats$mean_y_obs)
      grp2 = purrr::modify(rnorm(nr_samples$nr[2],
                                 0, mf[2]*model_stats$sd_resid),
                           ~ .x + model_stats$mean_y_obs)
    }

    tmp1 <- t.test(grp1, grp2, var.equal=TRUE)

    outcome[i,1] <- tmp1[[1]]
    outcome[i,2] <- tmp1[[3]]
    outcome[i,3] <- tmp1[[5]][1]-tmp1[[5]][2]
    }

    attr(outcome, "model_specifications") <- attributes(input_data)

    attr(outcome, "resampling_specifications") <- list(reps = reps,
                                                     sample_size = sample_size)

    attr(outcome, "test_stat") <- model_stats$diff_means[1]


    return(outcome)
    }
}

