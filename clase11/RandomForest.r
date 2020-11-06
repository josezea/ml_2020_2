

# f(x,y) = x %>% f(y)

library(AmesHousing)
library(ranger)
library(dplyr)

ames <- make_ames()
set.seed(123)

ames_train <- rsample::initial_split(ames, 0.7) %>% training()
set.seed(123)
ames_testing <- rsample::initial_split(ames, 0.7) %>% testing()

# number of features
n_features <- length(setdiff(names(ames_train), "Sale_Price"))


# train a default random forest model
ames_rf1 <- ranger(
  Sale_Price ~ ., 
  data = ames_train, 
  num.trees = 500,
  mtry = floor(n_features / 3),
  max.depth = 10, 
  respect.unordered.factors = "order",
  seed = 123
)



pred1 <- predict(ames_rf1, data = ames_testing)$predictions
plot(ames_testing$Sale_Price , pred1 )
abline(a = 0, b = 1, col = "red")
# RMSE
sqrt(mean((ames_testing$Sale_Price - pred1)^2))





# train a default random forest model
modelo2 <- ranger(
  Sale_Price ~ ., 
  data = ames_train, 
  num.trees = 450,
  mtry = n_features / 3,
  max.depth = 20, 
  respect.unordered.factors = "order",
  replace = T,
  seed = 123
)



pred2 <- predict(modelo2, data = ames_testing)$predictions
plot(ames_testing$Sale_Price , pred2 )
abline(a = 0, b = 1, col = "red")
# RMSE
sqrt(mean((ames_testing$Sale_Price - pred2)^2))




# create hyperparameter grid
hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)

# execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = Sale_Price ~ ., 
    data            = ames_train, 
    num.trees       = n_features * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
  )
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

# assess top 10 models
a <- hyper_grid %>%
  arrange(rmse) %>%
  head(10)



modelo3 <- ranger(
  Sale_Price ~ ., 
  data = ames_train, 
  num.trees = 500,
  mtry = 26,
  replace = F,
  sample.fraction = 0.8,
  max.depth = NULL, 
  respect.unordered.factors = "order",
  seed = 123
)


pred3 <- predict(modelo3, data = ames_testing)$predictions
plot(ames_testing$Sale_Price , pred3)
abline(a = 0, b = 1, col = "red")
# RMSE
sqrt(mean((ames_testing$Sale_Price - pred3)^2))
