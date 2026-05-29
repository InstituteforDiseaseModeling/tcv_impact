
#' The following code can be copied into the code chunk `final models` to generate single
#' best-fit models for different time points and age groups.
#' Contains models for:
#' - 0-2y impact, all ages
#' - 0-5y impact, all ages
#' - 0-2y impact, by age group
#' - single year (0-1y, 1-2y, 2-3y, 3-4y, 4-5y) impact, all ages
#' - single year (0-1y, 1-2y, 2-3y, 3-4y, 4-5y) impact, by age group
#' 
#' Be sure to add the results of any models you choose to run to the data frame `out`!

# 2-year impact, all ages
mod_2y_all = create_model("All", start_window=0, end_window=24, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
est_2y_all = get_model_output(mod_2y_all, age="All", mod_label=site_name)
est_2y_all$time_period = "2y"

# 5-year impact, all ages
mod_5y_all = create_model("All", start_window=0, end_window=60, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
est_5y_all = get_model_output(mod_5y_all, age="All", mod_label=site_name)
est_5y_all$time_period = "5y"

# 2-year impact, by age groups
mod_2y_a1 = create_model("0-2y", start_window=0, end_window=24, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
mod_2y_a2 = create_model("2-5y", start_window=0, end_window=24, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
mod_2y_a3 = create_model("5-15y", start_window=0, end_window=24, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
mod_2y_a4 = create_model("15y+", start_window=0, end_window=24, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)

est_2y_a1 = get_model_output(mod_2y_a1, age="0-2y", mod_label=site_name)
est_2y_a2 = get_model_output(mod_2y_a2, age="2-5y", mod_label=site_name)
est_2y_a3 = get_model_output(mod_2y_a3, age="5-15y", mod_label=site_name)
est_2y_a4 = get_model_output(mod_2y_a4, age="15y+", mod_label=site_name)
est_2y_a1$time_period="2y"
est_2y_a2$time_period="2y"
est_2y_a3$time_period="2y"
est_2y_a4$time_period="2y"

# single year impacts, all ages
mod_1_all = create_model("All", start_window=0, end_window=12, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
mod_2_all = create_model("All", start_window=12, end_window=24, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
mod_3_all = create_model("All", start_window=24, end_window=36, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
mod_4_all = create_model("All", start_window=36, end_window=48, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
mod_5_all = create_model("All", start_window=48, end_window=60, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)

est_1_all = get_model_output(mod_1_all, age="All", mod_label=site_name)
est_2_all = get_model_output(mod_2_all, age="All", mod_label=site_name)
est_3_all = get_model_output(mod_3_all, age="All", mod_label=site_name)
est_4_all = get_model_output(mod_4_all, age="All", mod_label=site_name)
est_5_all = get_model_output(mod_5_all, age="All", mod_label=site_name)
est_1_all$time_period="0-1y"
est_2_all$time_period="1-2y"
est_3_all$time_period="2-3y"
est_4_all$time_period="3-4y"
est_5_all$time_period="4-5y"

# plot the 2y, 5y, per-year fits
plot_mod_fit(mod_2y_all, plot_title="2y, all ages")
plot_mod_fit(mod_5y_all, plot_title="5y, all ages")

plot_grid(
  plot_mod_fit(mod_1_all, "0-1y"),
  plot_mod_fit(mod_2_all, "1-2y"),
  plot_mod_fit(mod_3_all, "2-3y"),
  plot_mod_fit(mod_4_all, "3-4y"), # only plotting first 4 years
  nrow=2
)

out = bind_rows(out, est_2y_all, est_5y_all, est_2y_a1, est_2y_a2, est_2y_a3, est_2y_a4, est_1_all, est_2_all, est_3_all, est_4_all, est_5_all)

# single year impacts, by age groups
for(age in age_cat_labels){
  mod_1 = create_model(age, start_window=0, end_window=12, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
  mod_2 = create_model(age, start_window=12, end_window=24, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
  mod_3 = create_model(age, start_window=24, end_window=36, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
  mod_4 = create_model(age, start_window=36, end_window=48, incl_seculartrend = final_incl_sectrend, incl_seasonality = final_incl_season, incl_culture=final_incl_culture)
  
  est_1 = get_model_output(mod_1, age=age, mod_label=site_name)
  est_2 = get_model_output(mod_2, age=age, mod_label=site_name)
  est_3 = get_model_output(mod_3, age=age, mod_label=site_name)
  est_4 = get_model_output(mod_4, age=age, mod_label=site_name)
  est_1$time_period="0-1y"
  est_2$time_period="1-2y"
  est_3$time_period="2-3y"
  est_4$time_period="3-4y"
  
  out = bind_rows(out, est_1, est_2, est_3, est_4)
}

