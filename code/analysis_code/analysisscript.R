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
fig1 = mydata %>% ggplot(aes(x=Population, y=`Total Cases`, label=Country)) + geom_point() + 
  geom_text(aes(label=ifelse(`Total Cases`>20000000, as.character(Country),'')), hjust=.3,vjust=1) + 
            ggtitle("Figure 1: Scatterplot of Total Cases by Population for each Country \n Labeling some outliers")

#This scatter plot is in response to looking at only countries that has less than 40 million people. This is to
#show the distribution of the other countries.
fig2 = mydata %>% filter(Population <= 40000000) %>% ggplot(aes(x=Population, y=`Total Cases`)) + geom_point() + 
  ggtitle("Figure 2: Scatterplot of Total Cases by Population for each Country \n under the Population of 40 million")

#I wanted to see the top 10 Countries with the most cases proportionally to their population. I included the 
#continent that are on just to see if there was any connection. We can see that there is a good mixture of countries 
#within this top ten list
tab1 = mydata %>% mutate(totcase_percent = (`Total Cases`/Population)*100) %>% arrange(desc(totcase_percent)) %>% 
  select(Country,location, totcase_percent) %>% head(., 10)

#This is a scatterplot of showing total cases by total recovered. We can see just like the population scatterplot
#that the same three countries have much larger numbers than all of the other ones. Just by the look of this
#It looks to have a simple linear relationship. I wanted to see if there were any countries that could be doing the 
#recovery/health care process.
fig3 = mydata %>% ggplot(aes(y=`Total Recovered`, x=`Total Cases`, label = Country)) + geom_point() + 
  geom_text(aes(label = ifelse(`Total Cases` > 20000000, as.character(Country),''))) + 
  ggtitle("Figure 3: Scatterplot of Total Recovered by Total Cases \n with labels for Countries over 20 million")

#Just like before, I subsetted the countries to only those who had less than 20 million cases to look at the bulk
#of the countries and the distribution. We can see is it really a linear relationship for all countries and no countries
#have a lot more recovered cases.
fig4 = mydata %>% filter(`Total Cases` <= 20000000) %>% ggplot(aes(y=`Total Recovered`, x=`Total Cases`, label = Country)) + 
  geom_point() + geom_text(aes(label=ifelse(`Total Cases` > 3000000, as.character(Country),''))) + 
  ggtitle("Figure 4: Scatterplot of Total Recovered by Total Cases for only Countries under 20 million cases \n (labeling for total cases over 3 million)")

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

#This is a histogram showing the distribution of those who are fully vaccinated. We can see that it is right skewed
#Which is not a surprise but the majority of countries have less than 25% of people fully vaccinated.
fig5 = mydata %>% ggplot(aes(x = `% of population fully vaccinated`)) + geom_histogram(binwidth = 10) + 
  ggtitle("Figure 5: Histogram of Percent of People Fully Vaccinated \n (by Country)")
  
#This scatterplot is looking at the amount of health care funding a country has compared to the percent of people
#vaccinated within each country. Since there were only 48 countries that provided their health care funds, this is 
#only showing those countries. We can see that they is definitely some positive correlation between funds and people 
#vaccinated with the United States as the one country that looks to have more funding but not as much people vaccinated.
fig6 = mydata %>% ggplot(aes(x=Value, y=`% of population fully vaccinated`, label = Country)) + 
  geom_point() + geom_text(aes(label=Country)) + 
  ggtitle("Figure 6: Scatterplot of Percent of People Fully Vaccinated by Health Care Funds \n (by Country)") +
  xlab("Health Care Funds")

#This is a histogram of the difference in proportions of those who are recovered and who died with each denominator as
#number of cases.
fig7 = mydata %>% ggplot(aes(x = prop_diff_recov_vs_death )) + geom_histogram(binwidth = 0.05) + 
  ggtitle("Figure 7: Histogram of Difference in proportions \n of those who recovered versus those who died \n (by Country)")
fig7

#Looking at the distribution using a histogram
fig8 = mydata %>% ggplot(aes(x = pct_cases)) + geom_histogram(binwidth = 2) + 
  ggtitle("Figure 8: Histogram of Percentage of cases\n (by Country)")
fig8


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
fig1_file = here("results","figure1.png")
ggsave(filename = fig1_file, plot=fig1) 

fig2_file = here("results","figure2.png")
ggsave(filename = fig2_file, plot=fig2) 

fig3_file = here("results","figure3.png")
ggsave(filename = fig3_file, plot=fig3) 

fig4_file = here("results","figure4.png")
ggsave(filename = fig4_file, plot=fig4) 

fig5_file = here("results","figure5.png")
ggsave(filename = fig5_file, plot=fig5) 

fig6_file = here("results","figure6.png")
ggsave(filename = fig6_file, plot=fig6) 

fig7_file = here("results","figure7.png")
ggsave(filename = fig7_file, plot=fig7)

fig8_file = here("results","figure8.png")
ggsave(filename = fig8_file, plot=fig8)


######################################
#Data fitting/statistical analysis
######################################

#Models 1 and 11 are looking at making models using binomial and quasibinomial as their family types. Included were summary tables
#for each that show model information
model1 = glm(prop_diff_recov_vs_death ~ prop_vacc, weights = `Total Cases`, data = mydata, family = "binomial")
summary(model1)

model11 = glm(prop_diff_recov_vs_death ~ prop_vacc, weights = `Total Cases`, data = mydata, family = "quasibinomial")
summary(model11)






#Below starts the look at using a logit transformation of the outcome since the data was very skewed and the usage of binomial
#quasibinomial produced high deviances and dispersion values.

#Looking at the distribution of the proportion difference between proportion of recovered vs died
summary(mydata$prop_diff_recov_vs_death)

#Transforming my outcome to a logit value
mydata$logit_prop_diff = logit(mydata$prop_diff_recov_vs_death)

#This is a histogram of the logit difference in proportions of those who are recovered and who died with each denominator as
#number of cases.
fig9 = mydata %>% ggplot(aes(x = logit_prop_diff )) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Figure 9: Histogram of logit transformation of difference in proportions \n of those who recovered versus those who died \n (by Country)")
fig9

#Saving figure * to a location for future references
fig9_file = here("results","figure9.png")
ggsave(filename = fig9_file, plot=fig9)


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
tabhc1_file = here("results", "tablehc1.rds")
saveRDS(hc_fund_stats, file = tabhc1_file)

tabvacc1_file = here("results", "tablevacc1.rds")
saveRDS(vacc_stats, file = tabvacc1_file)

tabtests1_file = here("results", "tabletests1.rds")
saveRDS(tests_stats, file = tabtests1_file)

tabloc1_file = here("results", "tableloc1.rds")
saveRDS(loc_stats, file = tabloc1_file)


#After looking at the models, it looks like the difference between the proportion of recovery and deaths
#are not affected by the predictor variables. After finding these results and looking at the data, the problem might live within
#the fact that over half of the difference in proportions are over 0.9. 

#Therefore, my next outcome I will look at will be just the case percentage for each country.





######### Analysis Part 2- percentage of cases within a country ###############


#Creating a recipe for each simple linear regression
vacc_rec2 = recipe(pct_cases~ `% of population vaccinated`, data = mydata)
healthcare_fund_rec2 = recipe(pct_cases~Value, data = mydata)
tests_rec2 = recipe(pct_cases~test_per_person, data = mydata)
loc_rec2 = recipe(pct_cases~location, data = mydata)

#Creating workflows based on the different recipes above
vacc_wrkflow2 <- workflow() %>% add_model(lm_mod) %>% add_recipe(vacc_rec2)
healthcare_fund_wrkflow2 <- workflow() %>% add_model(lm_mod) %>% add_recipe(healthcare_fund_rec2)
tests_wrkflow2 <- workflow() %>% add_model(lm_mod) %>% add_recipe(tests_rec2)
loc_wrkflow2 <- workflow() %>% add_model(lm_mod) %>% add_recipe(loc_rec2)

#Creating fit objects
vacc_fit2 <- vacc_wrkflow2 %>% fit(data = mydata)
hc_fund_fit2 <- healthcare_fund_wrkflow2 %>% fit(data = mydata)
tests_fit2 <- tests_wrkflow2 %>% fit(data = mydata)
loc_fit2 <- loc_wrkflow2 %>% fit(data = mydata)

#Looking at the details of each fitted model
vacc_fit2 %>% extract_fit_parsnip() %>% tidy()
hc_fund_fit2 %>% extract_fit_parsnip() %>% tidy()
tests_fit2 %>% extract_fit_parsnip() %>% tidy()
loc_fit2 %>% extract_fit_parsnip() %>% tidy()

vacc_stats2 = glance(vacc_fit2)
hc_fund_stats2 = glance(hc_fund_fit2)
tests_stats2 = glance(tests_fit2)
loc_stats2 = glance(loc_fit2)

#Saving tables for later use
tabhc2_file = here("results", "tablehc2.rds")
saveRDS(hc_fund_stats2, file = tabhc2_file)

tabvacc2_file = here("results", "tablevacc2.rds")
saveRDS(vacc_stats2, file = tabvacc2_file)

tabtests2_file = here("results", "tabletests2.rds")
saveRDS(tests_stats2, file = tabtests2_file)

tabloc2_file = here("results", "tableloc2.rds")
saveRDS(loc_stats2, file = tabloc2_file)

#Visualization of the simple linear regression models
#Simple scatterplots of these simple linear regressions
vacc_scatterplot <- mydata %>% ggplot(aes(x=`% of population vaccinated`, y = pct_cases)) + geom_point() + 
  ggtitle("Figure 10: Scatterplot of Percent of People Fully Vaccinated and Percentage of Cases \n (by Country)") 
vacc_scatterplot

hc_fund_scatterplot <- mydata %>% ggplot(aes(x=Value, y = pct_cases)) + geom_point() + 
  ggtitle("Figure 11: Scatterplot of Healthcare funds and Percentage of Cases \n (by Country)") +
  xlab("Health Care Funds")
hc_fund_scatterplot

tests_scatterplot <- mydata %>% ggplot(aes(x=test_per_person, y = pct_cases)) + geom_point()
tests_scatterplot

tests_scatterplot2 <- mydata %>% filter(test_per_person < 2) %>% ggplot(aes(x=test_per_person, y = pct_cases)) + geom_point()  + 
  ggtitle("Figure 12: Scatterplot of COVID-19 tests per person and Percentage of Cases \n (by Country)") 
tests_scatterplot2

#Simple boxplot of difference in proportions by continent
loc_boxplot <- mydata %>% ggplot(aes(x=location, y = pct_cases)) + geom_boxplot()  + 
  ggtitle("Figure 13: Histogram of the percent of cases by location \n (for each Country)") +
  xlab("Continent")
loc_boxplot

#Save figures to result folder
fig10_file = here("results","figure10.png")
ggsave(filename = fig10_file, plot=vacc_scatterplot)

fig11_file = here("results","figure11.png")
ggsave(filename = fig11_file, plot=hc_fund_scatterplot)

fig12_file = here("results","figure12.png")
ggsave(filename = fig12_file, plot=tests_scatterplot2)

fig13_file = here("results","figure13.png")
ggsave(filename = fig13_file, plot=loc_boxplot)
