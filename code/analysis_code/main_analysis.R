##Main Analysis

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse)
library(AER)
library(tidymodels)
library(dotwhisker)




#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######### Analysis Part 2- percentage of vaccinated people within a country ###############

## Correlation Matrix of all predictors ##

model_num_data <- mydata %>% select(prop_vacc, pct_cases, test_per_person, Value, gdp_per_capita)
cor_tab = cor(model_num_data, use = "complete.obs")
cor_file = here("results", "cor_tab.rds")
saveRDS(cor_tab, file = cor_file)


## Since health care funds (Value) and gdp have a correlation of 0.87 and there are a lot of missing data within health care funds,
## I will remove health care funds and replace it with gdp for my full model





####################################################################
######### Visualization of main outcome to predictors ##############
####################################################################

#Visualization of the simple linear regression models

#This is a table showing the top 5 and bottom 5 percentage of vaccinated people 
vacc_tab = mydata %>% select(Country, location, `% of population vaccinated`) %>% 
  arrange(desc(`% of population vaccinated`)) %>% {rbind(head(., 5), tail(., 5))}
vacc_file = here("results", "vacc_tab.rds")
saveRDS(vacc_tab, file = vacc_file)

#This is a histogram showing the distribution of those who are vaccinated. We can see that it is somewhat uniform
vacc_hist = mydata %>% ggplot(aes(x = `% of population vaccinated`)) + geom_histogram(binwidth = 5) + 
  ggtitle("Figure 2.1: Histogram of Percent of People Vaccinated \n (by Country)")
figvacc_file = here("results", "vacc_hist.png")
ggsave(filename = figvacc_file, plot= vacc_hist)


#Scatterplots of these simple linear regressions
case_scatterplot <- mydata %>% ggplot(aes(y=prop_vacc, x = pct_cases)) + geom_point() + 
  ggtitle("Scatterplot of percentage of cases and \n proportion of people vaccinated \n (by Country)") 
fig10_file = here("results/supp_mat","cases_vs_vacc.png")
ggsave(filename = fig10_file, plot=case_scatterplot)

gdp_per_cap_scatterplot <- mydata %>% ggplot(aes(x=gdp_per_capita, y = prop_vacc)) + geom_point() + 
  ggtitle("Figure 2.2: Scatterplot of GPD per capita and proportion of \n people vaccinated (by Country)") +
  xlab("GDP per capita") + ylab("Proportion vaccinated")
fig11_file = here("results","gdp_vs_vacc.png")
ggsave(filename = fig11_file, plot=gdp_per_cap_scatterplot)

tests_scatterplot <- mydata %>% ggplot(aes(x=test_per_person, y = prop_vacc)) + geom_point()
tests_scatterplot

tests_scatterplot2 <- mydata %>% filter(test_per_person < 2) %>% ggplot(aes(x=test_per_person, y = prop_vacc)) + geom_point()  + 
  ggtitle("Scatterplot of COVID-19 tests per person and proportion of people vaccinated \n (by Country)") 
fig12_file = here("results","test_vs_vacc.png")
ggsave(filename = fig12_file, plot=tests_scatterplot2)

#Simple boxplot of proportion of population vaccinated by continent
loc_boxplot <- mydata %>% ggplot(aes(x=location, y = prop_vacc)) + geom_boxplot()  + 
  ggtitle("Figure 2.3: Boxplot of the proportion of people vaccinated \n by location (for each Country)") +
  xlab("Continent") + ylab("Proportion vaccinated")
fig13_file = here("results","loc_vs_vacc.png")
ggsave(filename = fig13_file, plot=loc_boxplot)

#Simple boxplot of proportion of population vaccinated by government type
govt_boxplot <- mydata %>% ggplot(aes(x=government, y = prop_vacc)) + geom_boxplot()  + 
  ggtitle("Boxplot of the proportion of people vaccinated by government type \n (for each Country)") +
  xlab("Type of Government")
fig14_file = here("results", "govt_vs_vacc.png")
ggsave(filename = fig14_file, plot=govt_boxplot)

#Scatterplot of logit transformation of proportion of vaccinated people per country
log_vacc_hist <- mydata %>% ggplot(aes(x = logit_vacc)) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Figure 2.4: Histogram of the logit transformation \n of proportion of people vaccinated (by country)") + 
  xlab("Logit transformed vaccination proportion")
fig15_file = here("results", "log_vacc.png")
ggsave(filename = fig15_file, plot= log_vacc_hist)

################################################################################################





######################################################################################################
######### Analysis Part 2- percentage of cases within a country (no transformation) ##################
######################################################################################################

## Creating model types of tidymodeling

lm_mod <- linear_reg() %>% set_engine("lm")

## Simple linear regression without any transformation ##

#Creating a recipe for each simple linear regression
pctcases_rec2 = recipe(prop_vacc ~ pct_cases, data = mydata)
gdp_per_cap_rec2 = recipe(prop_vacc~gdp_per_capita, data = mydata)
tests_rec2 = recipe(prop_vacc~test_per_person, data = mydata)
loc_rec2 = recipe(prop_vacc~location, data = mydata)

#Creating workflows based on the different recipes above
pctcases_wrkflow2 <- workflow() %>% add_model(lm_mod) %>% add_recipe(pctcases_rec2)
gdp_per_cap_wrkflow2 <- workflow() %>% add_model(lm_mod) %>% add_recipe(gdp_per_cap_rec2)
tests_wrkflow2 <- workflow() %>% add_model(lm_mod) %>% add_recipe(tests_rec2)
loc_wrkflow2 <- workflow() %>% add_model(lm_mod) %>% add_recipe(loc_rec2)

#Creating fit objects
pctcases_fit2 <- pctcases_wrkflow2 %>% fit(data = mydata)
gdp_per_cap_fit2 <- gdp_per_cap_wrkflow2 %>% fit(data = mydata)
tests_fit2 <- tests_wrkflow2 %>% fit(data = mydata)
loc_fit2 <- loc_wrkflow2 %>% fit(data = mydata)

#Looking at the details of each fitted model
pctcases_fit2 %>% extract_fit_parsnip() %>% tidy()
gdp_per_cap_fit2 %>% extract_fit_parsnip() %>% tidy()
tests_fit2 %>% extract_fit_parsnip() %>% tidy()
loc_fit2 %>% extract_fit_parsnip() %>% tidy()

pctcases_stats2 = glance(pctcases_fit2)
gdp_per_cap_stats2 = glance(gdp_per_cap_fit2)
tests_stats2 = glance(tests_fit2)
loc_stats2 = glance(loc_fit2)






######################################################################################################
######### Analysis Part 2- percentage of cases within a country (logit transformation) ###############
######################################################################################################

#Creating a recipe for each simple linear regression (with weights)
pctcases_rec3 = recipe(logit_vacc ~ pct_cases, data = mydata)
gdp_per_cap_rec3 = recipe(logit_vacc ~gdp_per_capita, data = mydata)
tests_rec3 = recipe(logit_vacc~test_per_person, data = mydata)
loc_rec3 = recipe(logit_vacc~location, data = mydata)
govt_rec = recipe(logit_vacc~government, data = mydata)

#Creating workflows based on the different recipes above
pctcases_wrkflow3 <- workflow() %>% add_model(lm_mod) %>% add_recipe(pctcases_rec3)
gdp_per_cap_wrkflow3 <- workflow() %>% add_model(lm_mod) %>% add_recipe(gdp_per_cap_rec3)
tests_wrkflow3 <- workflow() %>% add_model(lm_mod) %>% add_recipe(tests_rec3)
loc_wrkflow3 <- workflow() %>% add_model(lm_mod) %>% add_recipe(loc_rec3)
govt_wrkflow <- workflow() %>% add_model(lm_mod) %>% add_recipe(govt_rec)

#Creating fit objects
pctcases_fit3 <- pctcases_wrkflow3 %>% fit(data = mydata)
gdp_per_cap_fit3 <- gdp_per_cap_wrkflow3 %>% fit(data = mydata)
tests_fit3 <- tests_wrkflow3 %>% fit(data = mydata)
loc_fit3 <- loc_wrkflow3 %>% fit(data = mydata)
govt_fit <- govt_wrkflow %>% fit(data=mydata)

#Looking at the details of each fitted model
pctcases_fit3 %>% extract_fit_parsnip() %>% tidy()
gdp_per_cap_fit3 %>% extract_fit_parsnip() %>% tidy()
tests_fit3 %>% extract_fit_parsnip() %>% tidy()
loc_fit3 %>% extract_fit_parsnip() %>% tidy()
govt_fit %>% extract_fit_parsnip() %>% tidy()

pctcases_stats3 = glance(pctcases_fit3)
gdp_per_cap_stats3 = glance(gdp_per_cap_fit3)
tests_stats3 = glance(tests_fit3)
loc_stats3 = glance(loc_fit3)
govt_stats = glance(govt_fit)

#Saving tables for later use
tabgdp2_file = here("results/supp_mat", "tablegdp2.rds")
saveRDS(gdp_per_cap_stats3, file = tabgdp2_file)

tabpctcases2_file = here("results/supp_mat", "tablepctcases2.rds")
saveRDS(pctcases_stats3, file = tabpctcases2_file)

tabtests2_file = here("results/supp_mat", "tabletests2.rds")
saveRDS(tests_stats3, file = tabtests2_file)

tabloc2_file = here("results/supp_mat", "tableloc2.rds")
saveRDS(loc_stats3, file = tabloc2_file)

tabgovt_file = here("results", "tablegovt.rds")
saveRDS(govt_stats, file = tabgovt_file)

#############################################################
############### Multivariate regression #####################
#############################################################

## First I am going to limit the data set to only variables I need for modeling

model_data <- mydata %>% select(logit_vacc, pct_cases, location, test_per_person, gdp_per_capita, prop_death, government) %>% 
  filter(complete.cases(.)) %>% mutate(government = as.factor(government),
                                       government = relevel(government, ref="Republic"))

mult_reg_rec1 = recipe(logit_vacc ~ ., data=model_data)
mult_reg_wrkflow1 <- workflow() %>% add_model(lm_mod) %>% add_recipe(mult_reg_rec1)
mult_reg_fit1 <- mult_reg_wrkflow1 %>% fit(data = model_data)
summ_tab_mult_reg <- mult_reg_fit1 %>% extract_fit_parsnip() %>% tidy()
mult_reg_stats1 = glance(mult_reg_fit1)

#Save table
tabML2_file = here("results", "Summ_stat_mult_reg.rds")
saveRDS(summ_tab_mult_reg, file = tabML2_file)

tabML_file = here("results", "Multiple_Reg_tab.rds")
saveRDS(mult_reg_stats1, file = tabML_file)



###########################################################
########### Cross-Validation and RMSE #####################
###########################################################

## Splitting data

#This is splitting the data using a proportion of 1/4 test and 3/4 training

#Setting seed
set.seed(345)

split_data <- initial_split(model_data, prop = 0.75)

#This is assigning acutal data sets to the ones that were made
training <- training(split_data)
test  <- testing(split_data)



## Workflow, fitting, and rmse 

#Creating recipe for full model
log_vacc_rec = recipe(logit_vacc ~ ., training) %>% step_dummy(all_nominal_predictors())

#Creating a workflow and adding the previous recipe
log_vacc_wrkflow <- workflow() %>% add_model(lm_mod) %>% add_recipe(log_vacc_rec)

#Creating a fit object to apply in the rmse
log_vacc_fit <- log_vacc_wrkflow %>% fit(data = training)

#Creating rmse values for both the training and test data
vacc_rmse_train <- augment(log_vacc_fit, training) %>% rmse(truth = logit_vacc, .pred)
vacc_rmse_test <- augment(log_vacc_fit, test) %>% rmse(truth = logit_vacc, .pred)

vacc_rmse = rbind(vacc_rmse_train, vacc_rmse_test)
tabrmse_file = here("results", "Logit_vacc_rmse_tab.rds")
saveRDS(vacc_rmse, file = tabrmse_file)




#Creating recipe for null model 
null_log_vacc_rec = recipe(logit_vacc ~ 1, training) %>% step_dummy(all_nominal_predictors())

#Creating a workflow and adding the previous recipe
null_log_vacc_wrkflow <- workflow() %>% add_model(lm_mod) %>% add_recipe(null_log_vacc_rec)

#Creating a fit object to apply in the rmse
null_log_vacc_fit <- null_log_vacc_wrkflow %>% fit(data = training)

#Creating rmse values for both the training and test data
null_vacc_rmse_train = augment(null_log_vacc_fit, training) %>% rmse(truth = logit_vacc, .pred)
null_vacc_rmse_test = augment(null_log_vacc_fit, test) %>% rmse(truth = logit_vacc, .pred)

null_vacc_rmse = rbind(null_vacc_rmse_train, null_vacc_rmse_test)
null_tabrmse_file = here("results", "Logit_vacc_rmse_null_tab.rds")
saveRDS(null_vacc_rmse, file = null_tabrmse_file)





##############################################################
################### LASSO Modeling ###########################
##############################################################

#creating a cross-validation 5-fold
cvfold_data = vfold_cv(training, v = 5, repeats = 5, strata = logit_vacc)

#Creating the lasso model type. This uses the glmnet engine
lr_mod <- 
  linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet") %>% set_mode("regression")

#We are setting the LASSO workflow where it uses the full model for the recipe and then the specific LASSO model I just made
lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(log_vacc_rec)

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

#Using tune grid to help create the tuning process using the cross-validation 5-fold
lr_res <- 
  lr_workflow %>% 
  tune_grid(cvfold_data,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse))

#Looking at all of the models created by the LASSO model
lr_res %>% 
  collect_metrics()

#The autoplot function shows basically the process of the tuning process
lasso_tune_plot <- lr_res %>% autoplot() + ggtitle("LASSO Tuning Process")
lasso_tune_file = here("results/supp_mat", "lasso_tune_process.png")
ggsave(filename = lasso_tune_file, plot= lasso_tune_plot)

#This is finding the best model with the lowest rmse
lr_best <-
  lr_res %>% 
  show_best("rmse", n=1)
lr_best

#Creating a workflow that only included the best model
final_wf_lr <- 
  lr_workflow %>% 
  finalize_workflow(lr_best)

#creating a fit object to help show the predictions of each logit transformed proportion vaccinated
lr_fit <- final_wf_lr %>% fit(data=training)

#Plotting the LASSO model
x <- lr_fit$fit$fit$fit
plot(x, "lambda")

#Creating the residuals and displaying the scatterplot of the predicted values to the observed values of logit proportion of vaccination using LASSO
aug_lr <- augment(lr_fit, training) %>% mutate(residual = logit_vacc - .pred)
lr_predvsobs_plot <- aug_lr %>% ggplot(aes(x=logit_vacc, y=.pred)) + geom_point() + geom_abline(intercept = 0, slope=1) + 
  ggtitle("Figure 2.5: Scatterplot of predicted values versus \n the observed values of logit transformed proportion of \n vaccinated people with a reference line showing \n exact prediction (for LASSO model)") + 
  xlab("Logit transformation of vaccination proportion") + ylab("Predicted values")
lr_predvsobs_plot
scatt_lasso_train_file = here("results", "Pred_vs_obs_value_lasso_train.png")
ggsave(filename = scatt_lasso_train_file, plot= lr_predvsobs_plot)

#Residual plot
lr_resid_plot <- aug_lr %>% ggplot(aes(x=.pred, y=residual)) + geom_point() + geom_hline(yintercept = 0) + 
  ggtitle("Figure 2.6: Residual plot for LASSO model (Training Data)") + xlab("Predicted values")
lr_resid_plot
resid_train_file = here("results", "Resid_lasso_train.png")
ggsave(filename = resid_train_file, plot= lr_resid_plot)


#comparing the rmse from the LASSO model to the null model
lr_res %>% show_best(n=1)
augment(null_log_vacc_fit, training) %>% rmse(truth = logit_vacc, .pred)




### Using the test data
lr_test_fit <- final_wf_lr %>% last_fit(split=split_data, metrics=metric_set(rmse))

#Comparing my test data rmse to the other best model data
rmse_train <- lr_res %>% show_best(n=1)
rmse_test<-lr_test_fit %>% 
  collect_metrics()
tabrmse1_file = here("results", "LASSO_rmse_train_tab.rds")
saveRDS(rmse_train, file = tabrmse1_file)
tabrmse2_file = here("results", "LASSO_rmse_test_tab.rds")
saveRDS(rmse_test, file = tabrmse2_file)


#Creating the augment to show the predicted values for each observation
lr_test_res <- lr_test_fit %>% augment()

#Scatterplot for predicted versus actual values
lr_predvsobs_test_plot <- lr_test_res %>% ggplot(aes(x=logit_vacc, y=.pred)) + geom_point() + geom_abline(intercept = 0, slope=1) + 
  ggtitle("Figure 2.7: Scatterplot of predicted values versus the \n observed values of logit transformed proportion of \n vaccinated people with a reference line showing \n exact prediction (for LASSO model using test data)") + 
  xlab("Logit transformation for vaccination proportion") + ylab("Predicted values")
lr_predvsobs_test_plot
scatt_lasso_test_file = here("results", "Pred_vs_obs_value_lasso_test.png")
ggsave(filename = scatt_lasso_test_file, plot= lr_predvsobs_test_plot)

#Residual plot
lr_resid_test_plot <- lr_test_res %>% ggplot(aes(x=.pred, y=.resid)) + geom_point() + geom_hline(yintercept = 0) + 
  ggtitle("Figure 2.8: Residual plot for LASSO model \n (using test data)") + xlab("Predicted values") + ylab("Residuals")
lr_resid_test_plot
resid_test_file = here("results", "Resid_lasso_test.png")
ggsave(filename = resid_test_file, plot= lr_resid_test_plot)

#Summary stat for LASSO model
lr_summ <- lr_fit %>% extract_fit_parsnip() %>% tidy() %>% filter(estimate != 0)
tablrsumm_file = here("results", "LASSO_summary_model.rds")
saveRDS(lr_summ, file = tablrsumm_file)

#Odds ratios for LASSO model 
Transform_LASSO <- lr_summ %>% mutate(Transformed_value = exp(estimate))
tabOR_file = here("results", "transform_LASSO.rds")
saveRDS(Transform_LASSO, file = tabOR_file)
