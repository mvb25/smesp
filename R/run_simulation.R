#' This function runs a simulation model multiple times and stores the statistics
#' of interest in a dataframe. The input data and model specifications are generated
#' by the function 'specify_simulation()'. In this function you define the
#' number pf replicates, the sample size in each simulation run, and the distribution
#' from which the x-values are randomly sampled.
#' @df input data. A data frame with one categorical variable with
#' two levels and two continuous variables
#' @reps number of replicates
#' @sample_size A number that gives the number of data points generated in the
#' simulation. The option 'as_data' returns the same number as in the original
#' data. The latter is default. In the case of 'difference between regression
#' slopes', the sample size per group will be equal or proportional to the sample
#' sizes in both groups. In the case of 'difference between sample_means', you can
#' set sample_size = "as data" or provide a vector with two numbers, which allows
#' for setting different sample sizes per group.
#' @xdistr Two options: "uniform" or "as data". These two options provide
#' two alternative probability distributions with which x-values are sampled
#' from the observed range of values (-/+ 5%). The option "uniform" speaks for
#' itself. Default option is "as data". This option computes a density curve of the
#' x-values, which is used to provide the probabilities with which x-values
#' are sampled.
#' @import readr
#' @import stringr
#' @import purrr
#' @import tidyr
#' @import tidyselect
#' @import broom
#' @import rsample
#' @import dplyr
#' @export
run_simulation <- function(
  df,
  reps         = 1000,
  sample_size  = "as data",
  xdistr       = "as data"){

#---regression slope------------------------------------------------------------
  if(attributes(df)$test == "slope"){

    # original data
    df <- df %>%
      rename(x_obs    = attributes(.)$predictor_variable,
             y_obs    = attributes(.)$response_variable) %>%
      mutate(y_pred   = predict(lm(y_obs ~ x_obs, data = .)),
             orig_res = y_pred-y_obs)

    # getting regression statistics of original data
    regr_model <- lm(y_obs ~ x_obs, data = df)

    # null hypothesis or confidence interval
    if(attributes(df)$procedure == "H0"){
      use_this_intercept <- mean(df$y_obs)
      use_this_slope     <- 0
    } else {
      use_this_intercept <- coefficients(regr_model)[1]
      use_this_slope     <- coefficients(regr_model)[2]
    }

    sd_orig_res <- df %>% summarise(tmp = sd(orig_res)) %>% as.numeric()

    if(sample_size == "as data"){nr_data_points = nrow(df)} else {nr_data_points = sample_size}

    simdat = list()

    for(i in 1:reps){

    # Getting predictor values and associated error terms
    if(xdistr == "uniform"){
      simdat[[i]] <- data.frame(sim_x    = seq(0.95*min(df$x_obs), 1.05*max(df$x_obs),
                                           length.out = max(1024, nr_data_points)),
                            heterosc = seq(1, attributes(df)$het_cont1,
                                           length.out = max(1024, nr_data_points))) %>%
        mutate(sdx = heterosc * (attributes(df)$error_cont1 * sd_orig_res) / mean(heterosc)) %>%
        slice_sample(n=nr_data_points, replace=T) %>%
        mutate(y_pop    = use_this_intercept + use_this_slope * sim_x,
               sim_y = y_pop + rnorm(nr_data_points, 0, sdx)) %>%
        select(sim_x, sim_y)

    } else if(xdistr == "as data") {
      # get coefficients of function of change in error term
      tmp <- data.frame(heterosc = seq(1, attributes(df)$het_cont1, length.out = nr_data_points)) %>%
        mutate(sim_x = seq(0.95*min(df$x_obs), 1.05*max(df$x_obs), length.out = nr_data_points),
               sdx   = heterosc * (attributes(df)$error_cont1 * sd_orig_res) / mean(heterosc)) %>%
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

    } else {stop("xdistr has to be 'as_data' or 'uniform")}
    }

    # Fitting a regression model on all data frames
    simdat <-  simdat %>% map(~ lm(sim_y ~ sim_x, data = .))


    # Extracting regression statistics
    tmp1 <- simdat %>%
      map_df(broom::tidy, .id = "id") %>%
      rename(replicate = id, p_value= p.value, se = std.error) %>%
      mutate(replicate = as.integer(replicate)) %>%
      mutate(term = rep(c("intercept ", "slope "),reps)) %>%
      na.omit()

    # Extracting R2 statistics
    tmp1 <- simdat %>%
      map_df(broom::glance, .id = "id") %>%
      mutate(replicate = as.integer(id),
             term = "R_adj",
             estimate = adj.r.squared,
             se = NA,
             statistic = NA,
             p_value = NA) %>%
      dplyr::select(replicate, term, estimate, se, statistic, p_value) %>%
      bind_rows(tmp1,.)


    attr(tmp1, "model_specifications") <- attributes(df)

    attr(tmp1, "resampling_specifications") <- list(reps        = reps,
                                                    xdistr     = xdistr,
                                                    sample_size = sample_size)

    attr(tmp1, "regr_model_original_data") <- list(tidy_output = broom::tidy(regr_model),
                                              glance_output = broom::glance(regr_model))

    attr(tmp1, "test_stat") <- unname(coefficients(regr_model)[2])

    return(tmp1)


#---difference between regression slopes----------------------------------------
  } else if(attributes(df)$test == "diff slopes"){

    # original data
    df <- df %>%
      rename(x_cont   = attributes(.)$continuous_predictor,
             x_cat    = attributes(.)$categorical_predictor,
             y_obs    = attributes(.)$response_variable) %>%
      mutate(y_pred   = predict(lm(y_obs ~ x_cont*x_cat, data = .)),
             orig_res = y_pred-y_obs)

    # Calculate the difference between group sds of residuals and that of the
    # overall mean
    overall_sd_resid <- sd(df$orig_res)

    group_sds <- df %>%
      group_by(x_cat) %>%
      summarise(sd_resid = sd(orig_res)) %>%
      mutate(mf_grp = sd_resid/overall_sd_resid)


    # Define the multiplication factors to calculate for each group how much more
    # the error term is than the observed overall sd of the residuals
    if(length(attributes(df)$error_cat) == 1){
      if(attributes(df)$error_cat == "as data"){
        mf <- c(group_sds$mf_grp[1], group_sds$mf_grp[2])
      } else {
        print("error_cat has to be a vector with 2 values, or 'as data'")
      }
    } else {mf <- c(attributes(df)$error_cat)}

    # groups
    group_data <- df %>% mutate(overall_sd = sd(orig_res)) %>%
      group_by(x_cat) %>%
      summarise(nr_original = n(),
                overall_sd = mean(overall_sd)) %>%
      ungroup() %>%
      # calculates the error terms for the two groups by multiplying mf with the
      # overall sd of the residuals
      mutate(grp_mf = mf,
             grp_err = grp_mf*overall_sd) %>%
      # Getting proportion of observations (data points) per group
      mutate(prop_obs = nr_original / sum(nr_original)) %>%
      # define number of observations (= # of data points) per group
      # Use ifelse because with case_when all RHSs must evaluate to the same type of vector.
      # and when sample_size = "as data", that is not possible
      mutate(nr_new = ifelse(sample_size == "as data", nr_original, prop_obs*sample_size))



    ### Getting simulated x and y values

    simdat = list()

    for(i in 1:reps){

    if(xdistr == "uniform"){

      # Categorical variable, level 1
      grp1_range <- df %>%
        filter(x_cat == group_data$x_cat[1]) %>%
        summarise(min = 0.95*min(x_cont), max = 1.05*max(x_cont))

      grp1 <- data.frame(x_cont = seq(grp1_range$min, grp1_range$max,
                                      length.out = max(1024, group_data$nr_new[1])),
                         heterosc = seq(1, attributes(df)$het_cont1,
                                        length.out = max(1024, group_data$nr_new[1]))) %>%
        mutate(sdx = heterosc * (attributes(df)$error_cont1 * group_data$grp_err[1]) / mean(heterosc)) %>%
        slice_sample(n = group_data$nr_new[1], replace=T) %>% select(-heterosc) %>%
        mutate(x_cat = group_data$x_cat[1])

      # Categorical variable, level 2
      grp2_range <- df %>%
        filter(x_cat == group_data$x_cat[2]) %>%
        summarise(min = 0.95*min(x_cont), max = 1.05*max(x_cont))

      grp2 <- data.frame(x_cont = seq(grp2_range$min, grp2_range$max,
                                      length.out = max(1024, group_data$nr_new[2])),
                         heterosc = seq(1, attributes(df)$het_cont1,
                                        length.out = max(1024, group_data$nr_new[2]))) %>%
        mutate(sdx = heterosc * (attributes(df)$error_cont1 * group_data$grp_err[2]) / mean(heterosc)) %>%
        slice_sample(n = group_data$nr_new[2], replace=T) %>% select(-heterosc) %>%
        mutate(x_cat = group_data$x_cat[2])

      # Categorical variable, join two levels
      xy_data <- bind_rows(grp1, grp2)


    } else if(xdistr == "as data") {
      # Categorical variable, level 1
      # figure out how to use map() here!
      x_dens_distr <- df %>%
        filter(x_cat == group_data$x_cat[1])

      x_dens_distr = density(x_dens_distr$x_cont,
                             n = max(1024, group_data$nr_new[1]),
                             from = 0.95*min(x_dens_distr$x_cont), to = 1.05*max(x_dens_distr$x_cont))

      grp1 <- data.frame(x_cont = x_dens_distr$x,
                         heterosc = seq(1, attributes(df)$het_cont1,
                                        length.out = max(1024, group_data$nr_new[1]))) %>%
        mutate(sdx = heterosc * (attributes(df)$error_cont1 * group_data$grp_err[1]) / mean(heterosc)) %>%
        slice_sample(n = group_data$nr_new[1], weight_by = x_dens_distr$y, replace=T) %>% select(-heterosc) %>%
        mutate(x_cat = group_data$x_cat[1])

      # Categorical variable, level 2
      x_dens_distr <- df %>%
        filter(x_cat == group_data$x_cat[2])

      x_dens_distr = density(x_dens_distr$x_cont,
                             n = max(1024, group_data$nr_new[2]),
                             from = 0.95*min(x_dens_distr$x_cont), to = 1.05*max(x_dens_distr$x_cont))

      grp2 <- data.frame(x_cont = x_dens_distr$x,
                         heterosc = seq(1, attributes(df)$het_cont1,
                                        length.out = max(1024, group_data$nr_new[2]))) %>%
        mutate(sdx = heterosc * (attributes(df)$error_cont1 * group_data$grp_err[2]) / mean(heterosc)) %>%
        slice_sample(n = group_data$nr_new[2], weight_by = x_dens_distr$y, replace=T) %>% select(-heterosc) %>%
        mutate(x_cat = group_data$x_cat[2])

      # Categorical variable, join two levels
      xy_data <- bind_rows(grp1, grp2)

    }

      # When null hypothesis: regression statistics of the model fitted on original
      # data with categorical variable but no interaction (i.e., no difference in slope)
      if(attributes(df)$procedure == "H0"){
        lm.tmp <- lm(y_obs ~ x_cont + x_cat, data = df)
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
      cat_var_levels <- df %>%
       distinct(x_cat) %>%
        pull(x_cat)

      tmp1 <- simdat %>%
        map_df(broom::tidy, .id = "id") %>%
        rename(replicate = id, p_value= p.value, se = std.error) %>%
        mutate(replicate = as.integer(replicate)) %>%
        mutate(term = rep(c(str_c("intercept ", cat_var_levels[1]),
                                 "intercept difference",
                                 str_c("slope ", cat_var_levels[1]),
                                 "slope difference"),
                          reps)) %>%
        na.omit()

      # Extracting R2 statistics
      tmp1 <- simdat %>%
        map_df(broom::glance, .id = "id") %>%
        mutate(replicate = as.integer(id),
               term = "R_adj",
               estimate = adj.r.squared,
               se = NA,
               statistic = NA,
               p_value = NA) %>%
        dplyr::select(replicate, term, estimate, se, statistic, p_value) %>%
        bind_rows(tmp1,.)


      attr(tmp1, "model_specifications") <- attributes(df)

      attr(tmp1, "resampling_specifications") <- list(reps        = reps,
                                                      xdistr     = xdistr,
                                                      sample_size = sample_size)

      regr_model <- lm(y_obs ~ x_cat*x_cont, data = df)

      attr(tmp1, "test_stat") <- unname(coefficients(regr_model)[4])

      attr(tmp1, "regr_model_original_data") <- list(tidy_output = broom::tidy(regr_model),
                                                     glance_output = broom::glance(regr_model))

      return(tmp1)

#---difference between intercepts (parallel slopes assumed)----------------------------------------
  } else if(attributes(df)$test == "diff intercepts"){

    # original data

    df <- df %>%
      rename(x_cont   = attributes(.)$continuous_predictor,
             x_cat    = attributes(.)$categorical_predictor,
             y_obs    = attributes(.)$response_variable) %>%
      mutate(y_pred   = predict(lm(y_obs ~ x_cont + x_cat, data = .)),
             orig_res = y_pred-y_obs)

    # Calculate the difference between group sds of residuals and that of the
    # overall mean
    overall_sd_resid <- sd(df$orig_res)

    group_sds <- df %>%
      group_by(x_cat) %>%
      summarise(sd_resid = sd(orig_res)) %>%
      mutate(mf_grp = sd_resid/overall_sd_resid)


    # Define the multiplication factors to calculate for each group how much more
    # the error term is than the observed overall sd of the residuals
    if(length(attributes(df)$error_cat) == 1){
      if(attributes(df)$error_cat == "as data"){
        mf <- c(group_sds$mf_grp[1], group_sds$mf_grp[2])
      } else {
        print("error_cat has to be a vector with 2 values, or 'as data'")
      }
    } else {mf <- c(attributes(df)$error_cat)}

    # groups
    group_data <- df %>% mutate(overall_sd = sd(orig_res)) %>%
      group_by(x_cat) %>%
      summarise(nr_original = n(),
                overall_sd = mean(overall_sd)) %>%
      ungroup() %>%
      # calculates the error terms for the two groups by multiplying mf with the
      # overall sd of the residuals
      mutate(grp_mf = mf,
             grp_err = grp_mf*overall_sd) %>%
      # Getting proportion of observations (data points) per group
      mutate(prop_obs = nr_original / sum(nr_original)) %>%
      # define number of observations (= # of data points) per group
      # Use ifelse because with case_when all RHSs must evaluate to the same type of vector.
      # and when sample_size = "as data", that is not possible
      mutate(nr_new = ifelse(sample_size == "as data", nr_original, prop_obs*sample_size))



    ### Getting simulated x and y values

    simdat = list()

    for(i in 1:reps){

      if(xdistr == "uniform"){

        # Categorical variable, level 1
        grp1_range <- df %>%
          filter(x_cat == group_data$x_cat[1]) %>%
          summarise(min = 0.95*min(x_cont), max = 1.05*max(x_cont))

        grp1 <- data.frame(x_cont = seq(grp1_range$min, grp1_range$max,
                                        length.out = max(1024, group_data$nr_new[1])),
                           heterosc = seq(1, attributes(df)$het_cont1,
                                          length.out = max(1024, group_data$nr_new[1]))) %>%
          mutate(sdx = heterosc * (attributes(df)$error_cont1 * group_data$grp_err[1]) / mean(heterosc)) %>%
          slice_sample(n = group_data$nr_new[1], replace=T) %>% select(-heterosc) %>%
          mutate(x_cat = group_data$x_cat[1])

        # Categorical variable, level 2
        grp2_range <- df %>%
          filter(x_cat == group_data$x_cat[2]) %>%
          summarise(min = 0.95*min(x_cont), max = 1.05*max(x_cont))

        grp2 <- data.frame(x_cont = seq(grp2_range$min, grp2_range$max,
                                        length.out = max(1024, group_data$nr_new[2])),
                           heterosc = seq(1, attributes(df)$het_cont1,
                                          length.out = max(1024, group_data$nr_new[2]))) %>%
          mutate(sdx = heterosc * (attributes(df)$error_cont1 * group_data$grp_err[2]) / mean(heterosc)) %>%
          slice_sample(n = group_data$nr_new[2], replace=T) %>% select(-heterosc) %>%
          mutate(x_cat = group_data$x_cat[2])

        # Categorical variable, join two levels
        xy_data <- bind_rows(grp1, grp2)


      } else if(xdistr == "as data") {
        # Categorical variable, level 1
        # figure out how to use map() here!
        x_dens_distr <- df %>%
          filter(x_cat == group_data$x_cat[1])

        x_dens_distr = density(x_dens_distr$x_cont,
                               n = max(1024, group_data$nr_new[1]),
                               from = 0.95*min(x_dens_distr$x_cont), to = 1.05*max(x_dens_distr$x_cont))

        grp1 <- data.frame(x_cont = x_dens_distr$x,
                           heterosc = seq(1, attributes(df)$het_cont1,
                                          length.out = max(1024, group_data$nr_new[1]))) %>%
          mutate(sdx = heterosc * (attributes(df)$error_cont1 * group_data$grp_err[1]) / mean(heterosc)) %>%
          slice_sample(n = group_data$nr_new[1], weight_by = x_dens_distr$y, replace=T) %>% select(-heterosc) %>%
          mutate(x_cat = group_data$x_cat[1])

        # Categorical variable, level 2
        x_dens_distr <- df %>%
          filter(x_cat == group_data$x_cat[2])

        x_dens_distr = density(x_dens_distr$x_cont,
                               n = max(1024, group_data$nr_new[2]),
                               from = 0.95*min(x_dens_distr$x_cont), to = 1.05*max(x_dens_distr$x_cont))

        grp2 <- data.frame(x_cont = x_dens_distr$x,
                           heterosc = seq(1, attributes(df)$het_cont1,
                                          length.out = max(1024, group_data$nr_new[2]))) %>%
          mutate(sdx = heterosc * (attributes(df)$error_cont1 * group_data$grp_err[2]) / mean(heterosc)) %>%
          slice_sample(n = group_data$nr_new[2], weight_by = x_dens_distr$y, replace=T) %>% select(-heterosc) %>%
          mutate(x_cat = group_data$x_cat[2])

        # Categorical variable, join two levels
        xy_data <- bind_rows(grp1, grp2)

      }

      # When null hypothesis: regression statistics of the model fitted on original
      # data with categorical variable but no interaction (i.e., no difference in intercept)
      if(attributes(df)$procedure == "H0"){
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
    simdat <-  simdat %>% map(~ lm(sim_y ~ x_cat + x_cont, data = .))



    # Extracting regression statistics
    cat_var_levels <- df %>%
      distinct(x_cat) %>%
      pull(x_cat)

    tmp1 <- simdat %>%
      map_df(broom::tidy, .id = "id") %>%
      rename(replicate = id, p_value= p.value, se = std.error) %>%
      mutate(replicate = as.integer(replicate)) %>%
      mutate(term = rep(c(str_c("intercept ", cat_var_levels[1]),
                          "intercept difference",
                          str_c("slope")),
                        reps)) %>%
      na.omit()

    # Extracting R2 statistics
    tmp1 <- simdat %>%
      map_df(broom::glance, .id = "id") %>%
      mutate(replicate = as.integer(id),
             term = "R_adj",
             estimate = adj.r.squared,
             se = NA,
             statistic = NA,
             p_value = NA) %>%
      dplyr::select(replicate, term, estimate, se, statistic, p_value) %>%
      bind_rows(tmp1,.)


    attr(tmp1, "model_specifications") <- attributes(df)

    attr(tmp1, "resampling_specifications") <- list(reps        = reps,
                                                    xdistr     = xdistr,
                                                    sample_size = sample_size)

    regr_model <- lm(y_obs ~ x_cat*x_cont, data = df)

    attr(tmp1, "test_stat") <- unname(coefficients(regr_model)[4])

    attr(tmp1, "regr_model_original_data") <- list(tidy_output = broom::tidy(regr_model),
                                                   glance_output = broom::glance(regr_model))

    return(tmp1)


#---difference between means---------------------------------------------
  } else if(attributes(df)$test == "diff means"){

    # original data
    df <- df %>%
      rename(x_obs    = attributes(.)$predictor_variable,
             y_obs    = attributes(.)$response_variable)

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
      # use the observed difference in error term, use 'het_cat', based on
      # values calculated prior to using this function.
      ungroup() %>%
      mutate(sd_resid = sd(residual)) %>%
      summarise(mean_y_obs = mean(y_obs),
                sd_resid = mean(sd_resid)) %>%
      # multiply the error term with user-provided factor
      bind_cols(model_stats, .)

    # Define the size of the sample
    if(length(sample_size) == 1){
      if(sample_size == "as data"){
        nr_samples <- df %>% group_by(x_obs) %>% summarise(nr = n())
      } else {
        nr_samples <- data.frame(nr = c(sample_size, sample_size))
      }
    } else if(length(sample_size) == 2){
      nr_samples <- data.frame(nr = sample_size)
    }

    # Calculate the difference between group sds of residuals and that of the
    # overall mean
    group_sds <- df %>%
      group_by(x_obs) %>%
      mutate(means = mean(y_obs),
             resid = mean(y_obs)-y_obs) %>%
      summarise(mean = mean(means),
                sd_resid = sd(resid)) %>%
      mutate(mf_grp = sd_resid/model_stats$sd_resid[1])


    # extract the error_cat values from the attributes of the original data
    if(attributes(df)$error_cat == "as data"){
      mf <- c(group_sds$mf_grp[1], group_sds$mf_grp[2])
    } else{
      mf <- c(attributes(df)$error_cat)
    }

    outcome <- data.frame(t_value = numeric(reps), p_value = numeric(reps), diff_means = numeric(reps))

    for(i in 1:reps){
    # Get a random sample
    if(attributes(df)$procedure == "CI"){
      # sample error term and add group means
      grp1 = purrr::modify(rnorm(nr_samples$nr[1],
                                 0, mf[1]*model_stats$sd_resid),
                           ~ .x + model_stats[[1]])
      grp2 = purrr::modify(rnorm(nr_samples$nr[2],
                                 0, mf[2]*model_stats$sd_resid),
                           ~ .x + model_stats[[2]])

      } else if(attributes(df)$procedure == "H0"){

      # sample error term and add mean_y_obs
      grp1 = purrr::modify(rnorm(nr_samples$nr[1],
                                 0, mf[1]*model_stats$sd_resid),
                           ~ .x + model_stats$mean_y_obs)
      grp2 = purrr::modify(rnorm(nr_samples$nr[2],
                                 0, mf[2]*model_stats$sd_resid),
                           ~ .x + model_stats$mean_y_obs)
    }

    if(mf[1] == mf[2]){
        tmp1 <- t.test(grp1, grp2, var.equal=TRUE)
      } else{
        tmp1 <- t.test(grp1, grp2, var.equal=FALSE)
      }

    outcome[i,1] <- tmp1[[1]]
    outcome[i,2] <- tmp1[[3]]
    outcome[i,3] <- tmp1[[5]][1]-tmp1[[5]][2]
    }

    attr(outcome, "model_specifications") <- attributes(df)

    attr(outcome, "resampling_specifications") <- list(reps = reps,
                                                     sample_size = sample_size)

    attr(outcome, "test_stat") <- model_stats$diff_means[1]


    return(outcome)
    }
}

