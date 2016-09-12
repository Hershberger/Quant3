library(causalA16)
library(ggplot2)

set.seed(8)
data_for_figure_4_10 <- make_data_for_figure_4_10(1000)
calc_average_causal_effect_estimand(data_for_figure_4_10)
calc_naive_difference_in_means(data_for_figure_4_10)
calc_conditional_difference_in_means(data_for_figure_4_10, "s")
calc_conditional_difference_in_means(data_for_figure_4_10, "x")

simulations <- replicate(1000, {
  data_for_figure_4_10 <- make_data_for_figure_4_10(1000)
  c(calc_average_causal_effect_estimand(data_for_figure_4_10),
    calc_naive_difference_in_means(data_for_figure_4_10),
    calc_conditional_difference_in_means(data_for_figure_4_10, "s"),
    calc_conditional_difference_in_means(data_for_figure_4_10, "x"))
})

sim_data <- data.table(simulations)
setnames(sim_data, c("ACE_estimand", "naive_diff_in_means",
  "diff_in_means_cond_s", "diff_in_means_cond_x"))
sim_data[, `:=`(
  MAE_diff_in_means = abs(ACE_estimand - naive_diff_in_means),
  MAE_diff_in_means_cond_s = abs(ACE_estimand - diff_in_means_cond_s),

)]
