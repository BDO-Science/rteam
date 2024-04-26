library(tidyverse)
library(phenomix)
library(glmmTMB)

#carcass data
winter <- read.csv('phenomix/carcass.csv', check.names = FALSE) %>%
  gather(key = 'Year', value = Count, 2:22) %>%
  mutate(Week = week(as.Date(Day)),
         Year = as.integer(Year)) %>%
  group_by(Year, Week) %>%
  summarize(Count = sum(Count, na.rm = TRUE)) %>%
  filter(Count > 0, Year < 2023) %>%
  na.omit() %>%
  as.data.frame() #for some reason you have to do this before creating the data list

#the package likes years in a 0,1,2,3 format so they need to be converted
cov_dat = data.frame(nyear = unique(winter$Year))
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear)

#data needs to be converted to a list for the model
datalist = create_data(winter,
                       min_number=0,
                       variable = "Count",
                       time="Year",
                       date = "Week",
                       asymmetric_model = FALSE,
                       mu = ~ nyear, #year as a covariate on the mean
                       sigma = ~ nyear, #and variance
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "student_t") #three curve fitting options (Gaussian, Student_t, gnorm)

#fit the model to the datalist
fitted <- fit(datalist)

#look at convergence which should be 0 if things properly converged
fitted$pars$convergence

# can look at variance estimates with sdrep
sdrep_df <- data.frame("par"=names(fitted$sdreport$value),
                      "value"=fitted$sdreport$value, "sd"=fitted$sdreport$sd)

# can pull out the AIC if you're testing multiple model fits
aic <- extractAIC(fitted)$AIC

#three ways we can get coefficients and uncertainty estimates out of model objects.
fixed <- fixef(fitted) #first way (requires installing glmmTMB)
random <- ranef(fitted) #second way (requires installing glmmTMB)
pars <- pars(fitted) #third way; call the pars function

#Graph with built in plot_diagnostics function
ex1 <- plot_diagnostics(fitted, type="timing", logspace=TRUE)
ex1

#Graph it out of logspace
ex2 <- plot_diagnostics(fitted, type="timing", logspace=FALSE)
ex2

#works as a ggplot object so you can manipulate graph
ex2 = plot_diagnostics(fitted, type="timing", logspace=TRUE) +
  facet_wrap(~years, scales = 'fixed') +
  labs(x = 'Week of Year', y = 'Observed and Predicted Count')
ex2

#you can pull the predicted dataframe
predicted <- predict(fitted, se.fit = TRUE)
view(predicted)

#might have to manipulate the year column
predicted <- predicted %>%
  mutate(years = (years+min(winter$Year))-1)
view(predicted)

#can extract summary stats for each year
m <- extract_means(fitted) %>% mutate(years = unique(winter$Year)) #extract means
t <- extract_theta(fitted) %>% mutate(years = unique(winter$Year))
l <- extract_lower(fitted) %>% mutate(years = unique(winter$Year)) #extract 25th quartile
u <- extract_upper(fitted) %>% mutate(years = unique(winter$Year)) #extract 75th quartile
all <- extract_all(fitted) #extract all parameters

#make your own graph with the predicted dataframe and the extracted summary statistics
ex3 <- ggplot(predicted, aes(x = x)) +
  geom_col(aes(y = y), fill = '#666666', alpha = 0.5) +
  geom_line(aes(y = pred), linewidth = 1, color = '#6699cc') +
  facet_wrap(~years) +
  geom_vline(data = m, aes(xintercept = value), linetype = 'dashed', linewidth = 1, alpha = 0.5) +
  labs(x = 'Week of Year', y = 'Count')
ex3

#take the predicted variable out of logspace
ex4 <- ggplot(predicted, aes(x = x)) +
  geom_col(aes(y = y), fill = '#666666', alpha = 0.5) +
  geom_line(aes(y = exp(pred)), linewidth = 1, color = '#6699cc') +
  facet_wrap(~years) +
  geom_vline(data = m, aes(xintercept = value), linetype = 'dashed', linewidth = 1, alpha = 0.5) +
  labs(x = 'Week of Year', y = 'Count')
ex4

#put your count data in the logspace
ex5 <- ggplot(predicted, aes(x = x)) +
  geom_col(aes(y = log(y)), fill = '#666666', alpha = 0.5) +
  geom_line(aes(y = pred), linewidth = 1, color = '#6699cc') +
  facet_wrap(~years) +
  geom_vline(data = m, aes(xintercept = value), linetype = 'dashed', linewidth = 1, alpha = 0.5) +
  labs(x = 'Week of Year', y = 'Ln(Count)')
ex5

#graph a scatter plot with the means to see if there was any noticeable trends over the years
ex6 <- ggplot(m, aes(x = years, y = value)) + geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
ex6

