###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse)
library(AER)
library(tidymodels)
library(caret)


#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data exploration/description
######################################
#The most notable of countries not in the data set are China and Philippines as they did not have any vaccine numbers 
#displayed within the source. 

#This is just to look at the distributions of each numeric variable
summarytable = summary(mydata)

#For this scatter plot, I plotted each countries population compare to their total cases of COVID-19.
#You can see that there are three countries that either has more population or total cases than others
case_vs_pop = mydata %>% ggplot(aes(x=Population, y=`Total Cases`, label=Country)) + geom_point() + 
  geom_text(aes(label=ifelse(`Total Cases`>20000000, as.character(Country),'')), hjust=.3,vjust=1) + 
            ggtitle("Scatterplot of Total Cases by Population for each Country \n Labeling some outliers")

#This scatter plot is in response to looking at only countries that has less than 40 million people. This is to
#show the distribution of the other countries.
case_vs_pop2 = mydata %>% filter(Population <= 40000000) %>% ggplot(aes(x=Population, y=`Total Cases`)) + geom_point() + 
  ggtitle("Scatterplot of Total Cases by Population for each Country \n under the Population of 40 million")

#I wanted to see the top 10 Countries with the most cases proportionally to their population. I included the 
#continent that are on just to see if there was any connection. We can see that there is a good mixture of countries 
#within this top ten list
tab1 = mydata %>% mutate(totcase_percent = (`Total Cases`/Population)*100) %>% arrange(desc(totcase_percent)) %>% 
  select(Country,location, totcase_percent) %>% head(., 10)

#This is a scatterplot of showing total cases by total recovered. We can see just like the population scatterplot
#that the same three countries have much larger numbers than all of the other ones. Just by the look of this
#It looks to have a simple linear relationship. I wanted to see if there were any countries that could be doing the 
#recovery/health care process.
recov_vs_case = mydata %>% ggplot(aes(y=`Total Recovered`, x=`Total Cases`, label = Country)) + geom_point() + 
  geom_text(aes(label = ifelse(`Total Cases` > 20000000, as.character(Country),''))) + 
  ggtitle("Scatterplot of Total Recovered by Total Cases \n with labels for Countries over 20 million")

#Just like before, I subsetted the countries to only those who had less than 20 million cases to look at the bulk
#of the countries and the distribution. We can see is it really a linear relationship for all countries and no countries
#have a lot more recovered cases.
recov_vs_case2 = mydata %>% filter(`Total Cases` <= 20000000) %>% ggplot(aes(y=`Total Recovered`, x=`Total Cases`, label = Country)) + 
  geom_point() + geom_text(aes(label=ifelse(`Total Cases` > 3000000, as.character(Country),''))) + 
  ggtitle("Scatterplot of Total Recovered by Total Cases for only Countries under 20 million cases \n (labeling for total cases over 3 million)")

#I then wanted to look at the recovery rate and which countries had the highest and lowest. Therefore, I used
#the head and tail functions to give the top 5 and bottom 5 recovery rate countries. Included in this table is also 
#the continent they are on, death rate, and total cases just to see if a lot of people were affected.
tab2 = mydata %>% mutate(recov_rate = `Total Recovered`/`Total Cases`, death_rate = `Total Deaths`/`Total Cases`) %>%
  select(Country, location, recov_rate, death_rate, `Total Cases`) %>% arrange(desc(recov_rate)) %>% 
  {rbind(head(., 5), tail(., 5))}

#I also want to look at the top 5 and bottom 5 death rate countries of COVID-19. We can see that two countries have over
#10% death rate of the total number of cases documented. However, Vanuatu only had 4 total cases of this. Therefore
#the population size does not really give a good representation. 
tab3 = mydata %>% mutate(recov_rate = `Total Recovered`/`Total Cases`, death_rate = `Total Deaths`/`Total Cases`) %>%
  select(Country, location, recov_rate, death_rate, `Total Cases`) %>% arrange(desc(death_rate)) %>% 
  {rbind(head(., 5), tail(., 5))}

#This scatterplot is looking at the amount of health care funding a country has compared to the percent of people
#vaccinated within each country. Since there were only 48 countries that provided their health care funds, this is 
#only showing those countries. We can see that they is definitely some positive correlation between funds and people 
#vaccinated with the United States as the one country that looks to have more funding but not as much people vaccinated.
vacc_vs_hc = mydata %>% ggplot(aes(x=Value, y=`% of population fully vaccinated`, label = Country)) + 
  geom_point() + geom_text(aes(label=Country)) + 
  ggtitle("Scatterplot of Percent of People Fully Vaccinated by Health Care Funds \n (by Country)") +
  xlab("Health Care Funds")

#This is a histogram of the difference in proportions of those who are recovered and who died with each denominator as
#number of cases.
prop_diff_recov_died = mydata %>% ggplot(aes(x = prop_diff_recov_vs_death )) + geom_histogram(binwidth = 0.05) + 
  ggtitle("Figure 1.1: Histogram of Difference in proportions \n of those who recovered versus those who died \n (by Country)") + 
  xlab("Proportion difference between recovery rates and death rates") + theme(plot.title = element_text(hjust=0.5))


#Looking at the distribution using a histogram
perc_case = mydata %>% ggplot(aes(x = pct_cases)) + geom_histogram(binwidth = 2) + 
  ggtitle("Histogram of Percentage of cases\n (by Country)")



#save data frame table to file for later use in manuscript
summarytable_file = here("results", "summarytable.rds")
saveRDS(summarytable, file = summarytable_file)

tab1_file = here("results", "table1.rds")
saveRDS(tab1, file = tab1_file)

tab2_file = here("results", "table2.rds")
saveRDS(tab2, file = tab2_file)

tab3_file = here("results", "table3.rds")
saveRDS(tab3, file = tab3_file)


#save figures
fig1_file = here("results","case_vs_pop.png")
ggsave(filename = fig1_file, plot=case_vs_pop) 

fig2_file = here("results","case_vs_pop2.png")
ggsave(filename = fig2_file, plot=case_vs_pop2) 

fig3_file = here("results","recov_vs_case.png")
ggsave(filename = fig3_file, plot=recov_vs_case) 

fig4_file = here("results","recov_vs_case2.png")
ggsave(filename = fig4_file, plot=recov_vs_case2) 

fig5_file = here("results","vacc_vs_hc.png")
ggsave(filename = fig5_file, plot=vacc_vs_hc) 

fig6_file = here("results","prop_diff_recov_died.png")
ggsave(filename = fig6_file, plot=prop_diff_recov_died) 

fig7_file = here("results","perc_case.png")
ggsave(filename = fig7_file, plot=perc_case)



######################################
#Data fitting/statistical analysis
######################################

#Models 1 and 11 are looking at making models using binomial and quasibinomial as their family types. Included were summary tables
#for each that show model information
model1 = glm(prop_diff_recov_vs_death ~ `% of population vaccinated`, weights = `Total Cases`, data = mydata, family = "binomial")
summary(model1)

model11 = glm(prop_diff_recov_vs_death ~ `% of population vaccinated`, weights = `Total Cases`, data = mydata, family = "quasibinomial")
summary(model11)






#Below starts the look at using a logit transformation of the outcome since the data was very skewed and the usage of binomial
#quasibinomial produced high deviances and dispersion values.

#Looking at the distribution of the proportion difference between proportion of recovered vs died
summary(mydata$prop_diff_recov_vs_death)

#Transforming my outcome to a logit value
mydata$logit_prop_diff = logit(mydata$prop_diff_recov_vs_death)

#This is a histogram of the logit difference in proportions of those who are recovered and who died with each denominator as
#number of cases.
log_prop_diff_hist = mydata %>% ggplot(aes(x = logit_prop_diff )) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Figure 1.2: Histogram of logit transformation of difference \n in proportions of those who recovered versus those who died \n (by Country)") + 
  xlab("Logit transformation of the proportional difference between recovory and death rates") + theme(plot.title = element_text(hjust=0.5))
log_prop_diff_hist

#Saving figure * to a location for future references
fig9_file = here("results","log_prop_diff_hist.png")
ggsave(filename = fig9_file, plot=log_prop_diff_hist)


#Creating a specific model type via tidymodels
lm_mod <- linear_reg() %>% set_engine("lm")

#Creating recipes
vacc_rec = recipe(logit_prop_diff~ `% of population vaccinated`, data = mydata)
healthcare_fund_rec = recipe(logit_prop_diff~Value, data = mydata)
tests_rec = recipe(logit_prop_diff~test_per_person, data = mydata)
loc_rec = recipe(logit_prop_diff~location, data = mydata)

#Creating workflows based on the different recipes above
vacc_wrkflow <- workflow() %>% add_model(lm_mod) %>% add_recipe(vacc_rec)
healthcare_fund_wrkflow <- workflow() %>% add_model(lm_mod) %>% add_recipe(healthcare_fund_rec)
tests_wrkflow <- workflow() %>% add_model(lm_mod) %>% add_recipe(tests_rec)
loc_wrkflow <- workflow() %>% add_model(lm_mod) %>% add_recipe(loc_rec)

#Creating fit objects
vacc_fit <- vacc_wrkflow %>% fit(data = mydata)
hc_fund_fit <- healthcare_fund_wrkflow %>% fit(data = mydata)
tests_fit <- tests_wrkflow %>% fit(data = mydata)
loc_fit <- loc_wrkflow %>% fit(data = mydata)

#Looking at the details of each fitted model
vacc_fit %>% extract_fit_parsnip() %>% tidy()
hc_fund_fit %>% extract_fit_parsnip() %>% tidy()
tests_fit %>% extract_fit_parsnip() %>% tidy()
loc_fit %>% extract_fit_parsnip() %>% tidy()

vacc_stats = glance(vacc_fit)
hc_fund_stats = glance(hc_fund_fit)
tests_stats = glance(tests_fit)
loc_stats = glance(loc_fit)

#Saving tables for later use
tabhc1_file = here("results/supp_mat", "tablehc1.rds")
saveRDS(hc_fund_stats, file = tabhc1_file)

tabvacc1_file = here("results", "tablevacc1.rds")
saveRDS(vacc_stats, file = tabvacc1_file)

tabtests1_file = here("results/supp_mat", "tabletests1.rds")
saveRDS(tests_stats, file = tabtests1_file)

tabloc1_file = here("results/supp_mat", "tableloc1.rds")
saveRDS(loc_stats, file = tabloc1_file)



#After looking at the models, it looks like the difference between the proportion of recovery and deaths
#are not affected by the predictor variables individually. After finding these results and looking at the data, the problem might live within
#the fact that over half of the difference in proportions are over 0.9. However, I will try a subset selection of manually selecting the predictors
#and see if a variable become significant.

#This is looking at correlations between some predictor variables and our outcome. 
analysis1_num_data <- mydata %>% select(prop_diff_recov_vs_death, prop_vacc, pct_cases, test_per_person, Value, gdp_per_capita)
cor_tab_prop_diff = cor(analysis1_num_data, use = "complete.obs")
cor_file_prop_diff = here("results/supp_mat", "cor_tab_prop_diff.rds")
saveRDS(cor_tab_prop_diff, file = cor_file_prop_diff)

#We can see that gdp per capita and value which is health care funds. Since health care funds has a lot of missing, 
#I will use the gdp per capita

#This is subsetting the variables needed for the full model of the logit prop diff
model_prop_diff_data <- mydata %>% select(logit_prop_diff, prop_vacc, pct_cases, location, test_per_person, gdp_per_capita, government) %>% 
  filter(complete.cases(.)) %>% mutate(government = as.factor(government),
                                       government = relevel(government, ref="Republic"))

#Creating the recipe for the full model
mult_reg_rec2 = recipe(logit_prop_diff ~ ., data=model_prop_diff_data)

#from the model type and recipe just created, I am making a workflow that allows me to make a fit object.
mult_reg_wrkflow2 <- workflow() %>% add_model(lm_mod) %>% add_recipe(mult_reg_rec2)
mult_reg_fit2 <- mult_reg_wrkflow2 %>% fit(data = model_prop_diff_data)

#We will be looking at the fitted model statistics
summ_tab_mult_reg2 <- mult_reg_fit2 %>% extract_fit_parsnip() %>% tidy()
mult_reg_stats2 = glance(mult_reg_fit2)

#Save table
tabML3_file = here("results", "Summ_stat_mult_reg2.rds")
saveRDS(summ_tab_mult_reg2, file = tabML3_file)

tabML4_file = here("results", "Multiple_Reg_tab2.rds")
saveRDS(mult_reg_stats2, file = tabML4_file)

########### Subset selection (using stepwise selection within the caret package) ###############

set.seed(340)

# Set up repeated 5-fold cross-validation
train.control <- trainControl(method = "cv", number = 5)
# Train the model
step.model <- train(logit_prop_diff ~., data = model_prop_diff_data,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:12),
                    trControl = train.control
)

#This is looking at the different models tests. Each nvmax represents the best model statistics for that number of variables included. 
#The way this function train works is they treat each level of predictor as one variable so even though I only have 6 predictors, there are 12
#different levels of predictors. 
step_proc = step.model$results
step_proc_file = here("results/supp_mat", "step_proc_tab.rds")
saveRDS(step_proc, file = step_proc_file)

#This shows which nvmax is created the best model
step.model$bestTune

#This shows the coefficients of the variables selection. As you can see, three variables were selected but two of them are location levels. 
coef(step.model$finalModel, 3)

### Since looking at the coefficients like the above code shows is strange, I created my own model using the variables selected. 


#Creating the recipe for the full model
stepwise_rec = recipe(logit_prop_diff ~ pct_cases + location, data=model_prop_diff_data)

#from the model type and recipe just created, I am making a workflow that allows me to make a fit object.
stepwise_wrkflow <- workflow() %>% add_model(lm_mod) %>% add_recipe(stepwise_rec)
stepwise_fit <- stepwise_wrkflow %>% fit(data = model_prop_diff_data)

#We will be looking at the fitted model statistics
summ_tab_stepwise <- stepwise_fit %>% extract_fit_parsnip() %>% tidy()
stepwise_stats = glance(stepwise_fit)

#Save table
tabstep1_file = here("results", "summ_stepwise_tab.rds")
saveRDS(summ_tab_stepwise, file = tabstep1_file)

tabstep2_file = here("results", "stepwise_stat_tab.rds")
saveRDS(stepwise_stats, file = tabstep2_file)

stepwise_model = lm(logit_prop_diff ~ pct_cases + location, data=model_prop_diff_data)

#Scatterplot for predicted versus actual values with a reference line of what a perfect prediction would be.
stepwise_predvsobs_plot = ggplot(aes(x=logit_prop_diff, y=stepwise_model$fitted.values), data = model_prop_diff_data) + geom_point() + geom_abline(intercept = 0, slope=1) + 
  ggtitle("Figure 1.3: Scatterplot of predicted values versus the \n observed values of logit transformed difference in proportions of \n recovered and deaths with a reference line showing exact prediction \n (Using stepwise selection)") + 
  xlab("Logit transformation for the difference in proportion of recovered versus death") + ylab("Predicted values") + theme(plot.title = element_text(hjust=0.5))
stepwise_predvsobs_plot
scatt_stepwise_file = here("results", "Pred_vs_obs_stepwise.png")
ggsave(filename = scatt_stepwise_file, plot= stepwise_predvsobs_plot)

#Residual plot with a reference line that would show a residual value of 0. 
stepwise_resid_plot <- ggplot(aes(x=stepwise_model$fitted.values, y=stepwise_model$residuals), data = model_prop_diff_data) + geom_point() + geom_hline(yintercept = 0) + 
  ggtitle("Figure 1.4: Residual plot for Stepwise model") + xlab("Predicted values") + ylab("Residuals") + theme(plot.title = element_text(hjust=0.5))
stepwise_resid_plot
resid_stepwise_file = here("results", "Resid_stepwise.png")
ggsave(filename = resid_stepwise_file, plot= stepwise_resid_plot)

#We can see that percent of cases and the location has significant impact on the logit transformed difference in proportions however, the r-squared
#values is very low. So even though the variables are significant, if we look at the predicted versus observed values and the residual plot,
#this is not a good fit.