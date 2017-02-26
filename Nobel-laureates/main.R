
#clear all lists
rm(list=ls())

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
##### which country has most laureates? which has least? ##
###########################################################

#use dplyr to arrange countries in descending order  
laureates.by.country = data.individuals %>%
  select(Birth.Country) %>% 
  group_by(Birth.Country) %>% 
  summarise(number = n()) %>% 
  arrange(-number)

###########################################################
### which country has highest number of women laureates? ##
###########################################################

laureates.by.sex = data.individuals %>%
  select(Sex) %>%
  group_by(Sex) %>%
  summarise( number = n() ) %>%
  arrange (-number)

#by country and by sex
laureates.by.sex.by.country = data.individuals %>%
  select(Sex, Birth.Country) %>%
  group_by(Sex, Birth.Country) %>%
  summarise( number = n() ) %>%
  arrange (-number)

#tidy data
laureates.by.sex.by.country = spread(laureates.by.sex.by.country, key = "Sex", value = "number")

#substitute NAs
laureates.by.sex.by.country$Female[is.na(laureates.by.sex.by.country$Female)] <- 0
laureates.by.sex.by.country$Male[is.na(laureates.by.sex.by.country$Male)] <- 0

#remove all rows where Female == 0
laureates.by.sex.by.country = laureates.by.sex.by.country[ laureates.by.sex.by.country$Female > 0 , ]


##########################################################
#### what's the most common category for each country #####
##########################################################

categories.by.country = data.individuals %>%
  select(Birth.Country, Category) %>% 
  group_by(Birth.Country, Category) %>% 
  summarise(number = n()) %>% 
  filter(number == max(number))

##########################################################
#### what categories have laureates most commonly shared  ###
##########################################################
#in future: display as percentage of total

#find number that follows a forward slash

shared.laureates = data.individuals
shared.laureates$Prize.Share = substring(shared.laureates$Prize.Share, 3)

shared.laureates$Prize.Share = as.integer(shared.laureates$Prize.Share)

#only display unique category, prize.share, year, country
#group by country and year, no summarise

shared.laureates = shared.laureates %>%
  select(Category, max(Prize.Share)) %>%
  group_by(Category)

shared.laureates = aggregate(shared.laureates$Prize.Share, list(shared.laureates$Category), max)
names(shared.laureates) = c("Category", "Max.share")

##########################################################
#### what universities are most common for laureates?  ###
##########################################################


laureates.by.university = data.individuals

laureates.by.university = laureates.by.university[ laureates.by.university$Organization.Name != "" , names(laureates.by.university) %in% c("Category", "Organization.Name") ]

laureates.by.university = laureates.by.university %>%
  select(Organization.Name, Category) %>% 
  group_by(Organization.Name, Category) %>% 
  summarise(number = n()) %>% 
  filter(number == max(number))

laureates.by.university = laureates.by.university %>%
  select(Category, max(number), Organization.Name) %>%
  group_by(Category) %>%
  filter(number == max(number))


###########################################################
#### who are the organizations that receieved a price? ####
###########################################################

laureates.organizations = data.organizations %>%
  select(Category) %>% 
  group_by(Category) %>% 
  summarise(number = n()) %>% 
  arrange(-number)



###########################################################
############### laureates' age distribution ###############
###########################################################

data.individuals$Birth.Date = as.Date(data.individuals$Birth.Date, format = "%Y-%m-%d")
data.individuals$Birth.Year = NULL
data.individuals$Birth.Year = format(data.individuals$Birth.Date, format = "%Y")
data.individuals$Birth.Year = as.integer(data.individuals$Birth.Year)

data.individuals$Age = NULL
data.individuals$Age = data.individuals$Year - data.individuals$Birth.Year

age.distribution = data.individuals %>%
  select(Age) %>%
  group_by(Age) %>%
  summarise( number = n()) %>%
  na.omit() %>%
  arrange (-number)

average.age.of.laureates = mean(age.distribution$Age)






