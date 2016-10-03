library(causalA16)
library(data.table)


estimates_experiment <- replicate(1000, {
  DATA <- simulate_simple_experiment(100)
  calc_naive_difference_in_means(DATA)
})

estimates_observational_study <- replicate(1000, {
  DATA <- simulate_observational_data(100)
  calc_naive_difference_in_means(DATA)
})

mean(estimates_experiment)
mean(estimates_observational_study)
