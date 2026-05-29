
#### Model initialization functions ####

#' This function creates the pre-post vaccine indicator, where
#' 0 indicates pre-vaccine time period and
#' 1 indicates the post-vaccine time period,
#' based on the provided input `date_prepost`.
#' This function should only be run once per case data frame.
create_prepost_ind <- function(df, date_prepost){
  df <- df %>%
    mutate(
      ind_prepost = as.numeric(date >= date_prepost), # pre 0, post 1
      fac_prepost = factor(ind_prepost, levels=0:1, labels=c("Pre", "Post"), ordered=T)
    ) %>%
    # calculate time since vaccine introduction (in years) 
    mutate(
      yr_since_vaccine = as.numeric(date - date_prepost)/365
    )
  return(df)  
}

#' This function creates the inclusion/exclusion indicator, where
#' 0 indicates these data rows should be excluded from analysis and
#' 1 indicates these data rows should be included in analysis,
#' based on the provided `start_excl` (date at which first data row should be excluded)
#' and `end_excl` (date at which last data row should be excluded).
#' This function can be run multiple times per data frame, if multiple periods
#' of exclusion are required.
#' Note this function requires the data frame to already have a column named `ind_include`.
create_inclusion_ind <- function(df, start_excl, end_excl){
  stopifnot("Data frame must have existing ind_include variable" = (!is.null(df$ind_include)))
  
  df <- df %>%
    mutate(
      tmp_ind_include = case_when(
        date < start_excl ~ 1,
        date >= start_excl & date < end_excl ~ 0,
        date >= end_excl ~ 1
      ),
      ind_include = ind_include * tmp_ind_include,
      fac_include = factor(ind_include, levels=0:1, labels=c("Exclude", "Include"), ordered=T)
    )
  return(df)  
}

#### Model fitting functions ####

#' This function estimates the 2 (or 3-4, if seasonality and culture adjustments are used) 
#' models described above for the given age group.
#' Arguments:
#' age: should be one of "All" (to estimate vaccine impact across all ages), or 
#'   "0-2y", "2-5y", "5-15y", or "15y+" to estimate age-specific impact
#' start_window: the starting month post-vaccination at which impact should be estimated. 
#'    By default, this is 0 (i.e. estimate for the first eligible month after vaccine introduction, 
#'    excluding any time periods defined above as excluded from analysis)
#' end_window: the ending month post-vaccination at which impact should be estimated. 
#'    By default, this is 24 (i.e., estimate up to 24 months, or 2 years, post-vaccination 
#'    introduction)
#' incl_seasonality: boolean, whether to fit a model which adjusts for seasonality
#' incl_culture: boolean, whether to fit a model which adjusts for volume of culture
#'    tests conducted
#' plot_output: if TRUE, will return figures of observed vs fitted incidence. If 
#'    FALSE, will return data frame of estimate vaccine impacts.
#' outcome_var: string, column name of the data to use as model outcome. By default,
#'    uses "typhoid_cases" to estimate impact of vaccination on all confirmed cases
estimate_impact<-function(
    age, 
    start_window=0, 
    end_window=24, 
    incl_seasonality=seasonality, 
    incl_culture=include_culture, 
    plot_output=FALSE, 
    outcome_var="typhoid_cases"
  ){
  
  stopifnot("Please provide a valid age value" = (age %in% c('All', age_cat_labels)))
  
  # unadjusted model
  m0 = create_model(age, start_window, end_window, incl_seculartrend = FALSE, incl_seasonality=FALSE, incl_culture=FALSE, outcome_var=outcome_var)
  
  # secular adjusted model
  m1 = create_model(age, start_window, end_window, incl_seculartrend = TRUE, incl_seasonality=FALSE, incl_culture=FALSE, outcome_var=outcome_var)
  
  if(incl_culture){
    # culture adjusted model
    m2 = create_model(age, start_window, end_window, incl_seculartrend = FALSE, incl_seasonality=FALSE, incl_culture=TRUE, outcome_var=outcome_var)
  }
  
  if(incl_seasonality){
    m3 = create_model(age, start_window, end_window, incl_seculartrend = FALSE, incl_seasonality=TRUE, incl_culture=FALSE, outcome_var=outcome_var)
  }
  
  if(plot_output){
    
    df_plot = m0$data %>%
      mutate(
        cases_m0 = m0$fitted.values,
        cases_m1 = m1$fitted.values
      )
    
    if(incl_culture){
      df_plot$cases_m2 = m2$fitted.values
      
    }
    if(incl_seasonality){
      df_plot$cases_m3 = m3$fitted.values
    }
    
    df_plot <- df_plot %>%
      pivot_longer(cols=c(oc, starts_with("cases_m")), values_to = "cases")
    
    df_plot = df_plot %>%
      mutate(
        inc_per_100k = cases / population * 100000 * 1/12, # 1/12 to convert to person-years
        mod_fac = factor(
          name, 
          levels=c("oc", "cases_m0", "cases_m1", "cases_m2", "cases_m3"),
          labels=c("Data", "Unadjusted", "Adjusted-Secular Trend", "Adjusted-Culture Count", "Adjusted-Seasonality"),
          ordered=T
        )
      )
    
    # add in any excluded months - to make clearer what was fit
    yrs = unique(df_plot$year)
    tmp <- expand_grid(
      month=1:12,
      year=yrs,
      mod_fac = unique(df_plot$mod_fac)
    ) %>%
      anti_join(df_plot) %>%
      mutate(inc_per_100k = NA, fac_prepost="Post")
    
    df_plot <- df_plot %>% bind_rows(tmp)
    
    p1 = ggplot(df_plot, aes(x=date, y=inc_per_100k, color=mod_fac)) +
      geom_line(aes(linetype=fac_prepost)) +
      theme_bw() +
      scale_color_manual(values=c("gray30", col5)) +
      labs(x="", y="Incidence per 100K person-years", color="Estimate", linetype="", title=age)
    
    return(p1)
  }else{
    out = bind_rows(
      get_model_output(m0, age, "Unadjusted"),
      get_model_output(m1, age, "Adjusted-Secular Trend")
    )
    
    if(incl_culture){
      out = bind_rows(
        out,
        get_model_output(m2, age, "Adjusted-Culture Count"),
      )
    }
    if(incl_seasonality){
      out = bind_rows(
        out,
        get_model_output(m3, age, "Adjusted-Seasonality")
      )
    }
    
    return(out)
  }
}


#' This is an internal function that creates and fits the model of interest, based
#' on the boolean arguments for which adjustments to include.
#' This gets called within estimate_impact to fit the multiple possible models of vaccine impact.
create_model <- function(age, start_window=0, end_window=24, incl_seculartrend=FALSE, incl_seasonality=FALSE, incl_culture=FALSE, outcome_var="typhoid_cases"){
  
  stopifnot("Please provide a valid age value" = (age %in% c('All', age_cat_labels)))
  
  if(age=='All'){
    fitdat <- df_allage %>% 
      filter(ind_include==1) %>%
      # retain all the pre-vaccine data + appropriate post-vaccine window
      filter(ind_prepost==0 | (yr_since_vaccine >= start_window/12 & yr_since_vaccine < end_window/12)) %>%
      # pull out outcome of interest
      mutate(oc = .[[outcome_var]])
  }else{
    fitdat <- df_ages %>% 
      filter(ind_include==1 & age_group==age) %>%
      # retain all the pre-vaccine data + appropriate post-vaccine window
      filter(ind_prepost==0 | (yr_since_vaccine >= start_window/12 & yr_since_vaccine < end_window/12)) %>%
      # pull out outcome of interest
      mutate(oc = .[[outcome_var]])
  }
  
  # confirm variables exist for define adjustments
  # otherwise make value NA to drop from model
  if(incl_seculartrend){
    timevar_exists = (!is.null(fitdat$time))
    stopifnot("Please ensure variable `time` is defined to adjust for secular trend" = timevar_exists)
  }
  
  if(incl_seasonality){
    seasvar_exists = mean(is.na(fitdat$seasonality_fac)) < 0.3 # no more than 30% missingness, arbitrary - should be 0% if defined correctly
    stopifnot("Please ensure variable `seasonality_fac` is defined to adjust for seasonality" = seasvar_exists)
  }
  
  if(incl_culture){
    cultvar_exists = mean(is.na(fitdat$culture_volume)) < 0.3 # no more than 30% missingness, arbitrary
    stopifnot("Please ensure variable `culture_volume` is defined to adjust for culture volume" = cultvar_exists)
  }
  
  # define the model string
  mod_str = "oc ~ ind_prepost"
  if(incl_seculartrend){mod_str = glue("{mod_str} + time")}
  if(incl_seasonality){mod_str = glue("{mod_str} + seasonality_fac")}
  if(incl_culture){mod_str = glue("{mod_str} + culture_volume")}
  
  # fit model - will drop the NA variables
  mod = glm(mod_str, data=fitdat, family='poisson', offset=log(population))
  
}

#' Reusable function to extract estimates of vaccine impact + 95% CI from a fitted model object
#' Returns a data frame with columns for VE, lower confidence interval (LCI), upper
#' confidence interval (UCI), model name or label, age group, and AIC
get_model_output <- function(mod, age, mod_label){
  mod_coef = mod$coefficients
  # calculating confidence intervals - returning NA if invalid model/CIs
  mod_ci <- tryCatch(
    confint(mod),
    error = function(e) NA
  )
  aic = AIC(mod)
  
  # exponentiate + convert to vaccine impact 1 - exp(beta)
  mn_imp = 1 - exp(mod_coef["ind_prepost"])
  if (all(is.na(mod_ci))) {
    lci <- NA
    uci <- NA
  } else {
    lci <- 1 - exp(mod_ci["ind_prepost", 2])
    uci <- 1 - exp(mod_ci["ind_prepost", 1])
  }
  
  out = data.frame(
    VE = mn_imp,
    LCL = lci,
    UCL = uci,
    model = mod_label,
    Age = age,
    AIC = aic
  )
  rownames(out)=NULL
  return(out)
}

#' Function to print a nice version of the output from `estimate_impact`
print_estimate_table <- function(ve_df){
  # if there is no column "time period", make it NA
  if(is.null(ve_df$time_period)){ve_df$time_period = NA_character_}
  
  ve_df %>%
    mutate(
      ve_lab = glue("{round(VE*100, 1)}%"),
      ci = glue("{round(LCL*100, 1)}, {round(UCL*100, 1)}")
    ) %>%
    select(time_period, Age, model, ve_lab, ci, AIC) %>%
    gt() %>%
    cols_label(
      time_period = "Period estimation",
      model = "Model",
      ve_lab = "Vaccine impact",
      ci = "95% CI"
    ) %>%
    fmt_number(columns = "AIC", decimals=4) %>%
    opt_interactive()
}

#' Function plots the fitted values (incidence) + CI of a single model versus the data used 
#' to fit the model, as a visual assessment of fit
plot_mod_fit <- function(mod, plot_title=NULL){
  
  mod_pred = predict(mod, se.fit=TRUE)
  
  df_plot = mod$data %>%
    mutate(
      # untransformed predictions/se
      fit_est = mod_pred$fit,
      fit_se = mod_pred$se.fit,
      lci = fit_est - qnorm(0.975)*fit_se,
      uci = fit_est + qnorm(0.975)*fit_se,
      # exponentiate
      fitted_count = exp(fit_est),
      fitted_lci = exp(lci),
      fitted_uci = exp(uci)
    ) %>%
    pivot_longer(cols=c(oc, fitted_count), values_to = "cases") %>%
    mutate(
      # drop CI values from data
      fitted_lci = if_else(name=="oc", NA, fitted_lci),
      fitted_uci = if_else(name=="oc", NA, fitted_uci)
    )
  
  df_plot = df_plot %>%
    mutate(
      inc_per_100k = cases / population * 100000 * 1/12, # 1/12 to convert to person-years
      inc_lci = fitted_lci / population * 100000 * 1/12,
      inc_uci = fitted_uci / population * 100000 * 1/12,
      mod_fac = factor(
        name, 
        levels=c("oc", "fitted_count"),
        labels=c("Data", "Modeled values"),
        ordered=T
      )
    )
  
  p1 = ggplot(df_plot, aes(x=date, y=inc_per_100k, color=mod_fac, fill=mod_fac, group=interaction(fac_prepost, mod_fac))) +
    geom_line(aes(linetype=fac_prepost)) +
    geom_ribbon(aes(ymin=inc_lci, ymax=inc_uci), alpha=0.25, linewidth=0) +
    geom_point(size=0.8) +
    theme_bw() +
    scale_color_manual(values=c("gray30", "#D59A6B")) +
    scale_fill_manual(values=c("gray30", "#D59A6B")) +
    labs(x="", y="Incidence per 100K person-years", color="Estimate", fill="Estimate", linetype="", title=plot_title)
  
  return(p1)
}


