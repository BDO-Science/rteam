library(tidyverse)
library(ggridges)

#set url for carcass data
url <- 'https://www.cbr.washington.edu/sacramento/data/php/rpt/carcass_detail.php?sc=1&outputFormat=csv&year=2023&run=winter&clip=all&sex=all&condition=all'

#read in data
data <- read_csv(url)
data2 <- read.csv(url)

#############
#filtering
#############

#single parameter filter
filter_base <- subset(data, clip == 'No Clip') #base R with subset
filter_base2 <- data[data$clip == 'No Clip',] #base R by subsetting

filter_tidyverse <- filter(data, clip == 'No Clip') #dplyr

#multi parameter filter with tidyverse
filter_tidyverse2 <- filter(data, clip == 'No Clip' & sex == 'F') #using the & operator
filter_tidyverse3 <- filter(data, surveydate < "2023-06-15" | surveydate > "2023-08-01") #using the OR operator

###################
#selecting columns
###################

select_base <- data[,c('surveydate', 'sex', 'forklength')] #selecting by index
select_base2 <- subset(data, select = c('surveydate', 'sex', 'forklength')) #selecting with subset

select_tidyverse <- select(data, c(surveydate, sex, forklength)) #selecting with tidyverse
select_tidyverse2 <- select(data, c(2,11,12)) #using column numbers instead of names
select_tidyverse3 <- select(data, c(date = 2, 11, FL = 12)) #changing column names while selecting

################
#basic chaining
################
chain_base <- mean(data[data$sex == 'F',]$forklength, na.rm = TRUE) #calculating mean fork length of females w/ baseR

chain_tidyverse <- data %>% #calculating mean fork length of females with tidyverse
  filter(sex == 'F') %>%
  summarize(avg_fl = mean(forklength, na.rm = TRUE))

chain_tidyverse2 <- data %>% #calculating mean fork length of females with tidyverse and pulling value
  filter(sex == 'F') %>%
  summarize(avg_fl = mean(forklength, na.rm = TRUE)) %>%
  pull()

################################
#chaining with transformation
################################
grouping_base <- aggregate(forklength ~ sex, data = data, FUN = mean)

grouping_tidyverse <- data %>%
  group_by(sex) %>%
  summarize(mean_fl = mean(forklength, na.rm = TRUE))

grouping_tidyverse2 <- data %>%
  filter(!is.na(sex)) %>%
  group_by(sex) %>%
  summarize(mean_fl = mean(forklength, na.rm = TRUE))

################################
#ggplot vs baseR plot
################################

##base R plotting
par(mfrow = c(1, 2)) # Set up the plotting area to have 2 plots side by side

# Histogram for Male
hist(data$forklength[data$sex == "M"],
     main = "Forklength for Males",
     xlab = "Forklength",
     col = "lightblue",
     border = "black")

# Histogram for Female
hist(data$forklength[data$sex == "F"],
     main = "Forklength for Females",
     xlab = "Forklength",
     col = "lightpink",
     border = "black")

##ggplot
graph <- ggplot(data, aes(x = forklength, fill = sex)) +
  geom_histogram(binwidth = 50, position = "identity", alpha = 0.6, color = "black") +
  labs(title = "Forklength for Males and Females", x = "Forklength", y = "Count") +
  scale_fill_manual(values = c("M" = "lightblue", "F" = "lightpink")) +
  facet_wrap(~sex) +
  theme_minimal()
graph


graph2 <- data %>%
  filter(sex == 'F') %>%
  group_by(surveydate) %>%
  summarize(n = n()) %>%
  mutate(cumul = cumsum(n)) %>%
  ggplot(aes(x = surveydate, y = cumul)) +
  geom_line()
graph2

graph3 <- data %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = forklength, y = sex, fill = sex)) +
  geom_density_ridges()
graph3
