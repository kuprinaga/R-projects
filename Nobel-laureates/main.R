
#clear all lists
rm(list=ls())

#answer questions:
#which country has highest number of women laureates?
#which country has most laureates in which category?
#which categories are most common to have 2 or more people share prize?
#which country has most prizes? which has least?
#what year had most categories with 2 or more people sharing prize?
#what universities are most common for laureates?
#who are the organizations that receieved a price?
#what's the most common age of laureates?

require(tidyverse)
require(stringr)

#read in files
data = read.csv("archive.csv", header = TRUE )

#only leave relevant columns
data = data[, names(data) %in% c("Year", "Category", "Prize.Share", "Laureate.Type", "Full.Name", "Birth.Date", "Birth.Country", "Sex", "Organization.Name", "Organization.Country") ]

#check for NAs
sapply(data, function(x) sum(is.na(x)))

#create a function which finds the real country name
realCountry = function(x){
  value = "\\(([^()]+)\\)"
  x = as.character(x)
  if(grepl("\\(",x) == TRUE) {
    y = gsub(value, "\\1", ignore.case = TRUE, str_extract_all(x, value)[[1]])
    return(as.character(y))
  }
  else
  {
      return(as.character(x))
    }
}

data$Birth.Country <- sapply(data$Birth.Country, realCountry)


#separate two lists
data.individuals = data [ data$Laureate.Type == "Individual" , ]
data.organizations = data [ data$Laureate.Type == "Organization" , ]

#answer questions related to individuals
###########################################################
##### which country has most prizes? which has least? #####
###########################################################

#use dplyr to arrange countries in descending order  
prizes.by.country = data.individuals %>%
  select(Birth.Country) %>% 
  group_by(Birth.Country) %>% 
  summarise(number = n()) %>% 
  arrange(-number)

###########################################################
### which country has highest number of women laureates? ##
###########################################################

prizes.by.sex = data.individuals %>%
  select(Sex) %>%
  group_by(Sex) %>%
  summarise( number = n() ) %>%
  arrange (-number)

#by country and by sex
prizes.by.sex.by.country = data.individuals %>%
  select(Sex, Birth.Country) %>%
  group_by(Sex, Birth.Country) %>%
  summarise( number = n() ) %>%
  arrange (-number)

#tidy data
prizes.by.sex.by.country = spread(prizes.by.sex.by.country, key = "Sex", value = "number")

#substitute NAs
prizes.by.sex.by.country$Female[is.na(prizes.by.sex.by.country$Female)] <- 0
prizes.by.sex.by.country$Male[is.na(prizes.by.sex.by.country$Male)] <- 0

#remove all rows where Female == 0
prizes.by.sex.by.country = prizes.by.sex.by.country[ prizes.by.sex.by.country$Female > 0 , ]


##########################################################
#### what's the most common category for each country #####
##########################################################

# find most common category for the country
categories.by.country = data.individuals %>%
  select(Birth.Country, Category) %>% 
  group_by(Birth.Country, Category) %>% 
  summarise(number = n()) %>% 
  filter(number == max(number))

##########################################################
#### what categories have prizes most commonly shared  ###
##########################################################
#display as percentage of total
#Wolf suspects: Medicine, Physics

#find number that follows a forward slash

shared.prizes = data.individuals
shared.prizes$Prize.Share = substring(shared.prizes$Prize.Share, 3)

shared.prizes$Prize.Share = as.integer(shared.prizes$Prize.Share)

#only display unique category, prize.share, year, country
#group by country and year, no summarise

shared.prizes = shared.prizes %>%
  select(Category, max(Prize.Share)) %>%
  group_by(Category)

shared.prizes = aggregate(shared.prizes$Prize.Share, list(shared.prizes$Category), max)
names(shared.prizes) = c("Category", "Max.share")
  
#only filter out unique values
shared.prizes = unique(shared.prizes)


