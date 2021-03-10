# KNN Modeling of COVID-19 spread in ICE immigration detention centers
# Camille Kirsch
# March 2021

library(tidyverse)
library(tidymodels)

covid <- read_csv("~cdkma/Documents/Machine Learning/data/final_data.csv")

# pre-process the data (convert cats to factors, drop a few columns)
covid_clean <- covid %>%
  select(-Facility, -total_cases, -per_day, -county, -county_cases, 
         -county_deaths) %>%
  mutate_if(is.character, as.factor)

# scale the data
covid_scaled <- covid_clean %>%
  mutate_if(is.factor, unclass) %>%
  mutate(across(.cols = c(2:17), scale))

#set seed
set.seed(591)

# train-test split
split <- initial_split(covid_scaled,
                       prop = 0.80) 
training <- training(split)
testing <- testing(split)

# 10-fold CV 
cv_splits <- vfold_cv(training, 
                     v = 10)

### Build KNN Model

# write recipe
recipe1 <- recipe(per_capita ~ ., 
                  data = covid_scaled)

# initialize a KNN model which tunes neighbors and weight
mod1 <- nearest_neighbor() %>% 
  set_args(neighbors = tune(),
           weight_func = tune()) %>%  
  set_engine("kknn") %>% 
  set_mode("regression")

# Start a workflow
flow1 <- workflow() %>%
  add_model(mod1) %>%
  add_recipe(recipe1)

#params <- parameters(flow1) %>%
#  update(K = neighbors(c(1:46)))

func_list = c("optimal", "rectangular", "triweight", "biweight", "inv", 
              "rank", "triangular", "gaussian", "cos", "epanechnikov")
covid_grid <- expand_grid(neighbors = c(1:46), 
                          weight_func = func_list)

output <- flow1 %>%
  tune_grid(resamples = cv_splits, 
            grid = covid_grid,
            metrics = metric_set(rmse, rsq), 
            control = control_grid(save_pred = TRUE))

output_summary <- output %>% 
  collect_metrics(summarize = TRUE) 
print(output_summary, n = nrow(output_summary))

estimates <- collect_metrics(output)
estimates

# make some graphs
library(RColorBrewer)
rmse_plot <- output_summary %>% 
  filter(.metric == "rmse") %>%
  ggplot(aes(neighbors, mean, 
             group = weight_func, color = weight_func)) +
  geom_line() +
  scale_color_brewer(palette = "Set3")+
  ylab("Mean RSME From 10-Fold CV") +
  xlab("Number of Neighbors") +
  ggtitle("Root Mean Square Error by Neighbors and Weight Function")
rmse_plot

rsq_plot <- output_summary %>% 
  filter(.metric == "rsq") %>%
  ggplot(aes(neighbors, mean, 
             group = weight_func, color = weight_func)) +
  geom_line() +
  scale_color_brewer(palette = "Set3") +
  ylab("Mean R-Squared From 10-Fold CV") +
  xlab("Number of Neighbors") +
  ggtitle("R-Squared by Neighbors and Weight Function")
rsq_plot

most_predictive <- output %>%
  select_best(metric = "rsq")
most_predictive

least_error <- output %>%
  select_best(metric = "rmse")
least_error

# based on the graphs above, we're going to go with k = 20, inverse distance.
# retrain the model on the full training set, then test on the testing set

mod2 <- nearest_neighbor() %>% 
  set_args(neighbors = 20,
           weight_func = "inv") %>%  
  set_engine("kknn") %>% 
  set_mode("regression")

fit2 <- workflow() %>%
  add_recipe(recipe1) %>%
  add_model(mod2) %>%
  fit(data = training)

fit2_summary <- fit2 %>%
  predict(testing) %>%
  bind_cols(testing) %>%
  metrics(truth = per_capita, estimate = .pred)

fit2_summary

# what if we went based on rsq instead of rmse?
# i.e., a model with k = 18, weight_func = "rectangular"

mod3 <- nearest_neighbor() %>% 
  set_args(neighbors = 18,
           weight_func = "rectangular") %>%  
  set_engine("kknn") %>% 
  set_mode("regression")

fit3 <- workflow() %>%
  add_recipe(recipe1) %>%
  add_model(mod3) %>%
  fit(data = training)

fit3_summary <- fit3 %>%
  predict(testing) %>%
  bind_cols(testing) %>%
  metrics(truth = per_capita, estimate = .pred)

fit3_summary
