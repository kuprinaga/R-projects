
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

laureates.by.country.chart = head(laureates.by.country)

list.of.top.countries = head(laureates.by.country$Birth.Country, n = 9L)
list.of.top.countries = as.list(list.of.top.countries)

ggplot (data = laureates.by.country.chart, aes(x = Birth.Country, y = number)) +
  geom_bar(stat = "identity", fill = "light blue") + 
  geom_text(aes(label= laureates.by.country.chart$Birth.Country), vjust = -1 )+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ylab("Number of Individual Laureates") +
  xlab("Country of Birth") +
  ggtitle("Number of Laureates by Country Compared to Average, top countries") +
  geom_hline(yintercept = mean(laureates.by.country$number), color="black")


###########################################################
### which country has highest number of women laureates? ##
###########################################################

laureates.by.sex = data.individuals %>%
  select(Sex) %>%
  group_by(Sex) %>%
  summarise( number = n() ) %>%
  arrange (-number) %>%
  mutate( percentage = number / sum(number) * 100 )

require(cowplot)
plot = ggplot (data = laureates.by.sex, aes(x = 1, y = percentage, fill=Sex)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5), axis.ticks.y= element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()) +
  xlab("Percentage") +
  ggtitle("Individual Laureates Grouped by Gender")


ggdraw(add_sub(plot, "95% of Laureates are Male"))

#by country and by sex
laureates.by.sex.by.country = data.individuals %>%
  select(Sex, Birth.Country) %>%
  group_by(Sex, Birth.Country) %>%
  summarise( number = n() ) %>%
  arrange (-number) %>%
  na.omit()

short.list.by.sex.by.country = laureates.by.sex.by.country[ laureates.by.sex.by.country$Birth.Country %in% list.of.top.countries , ]

ggplot (data = short.list.by.sex.by.country, aes(x = Sex, y = number, fill = Birth.Country)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.title.y =element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.y = element_blank(), axis.ticks.x = element_blank())+
  guides(fill=guide_legend(title="Top Countries")) +
  scale_fill_brewer(type = "seq", palette = "BuPu") +
  ggtitle("Gender Distribution by Country, percentage")
  
  
  
  ##########################################################
#### what's the most common category for each country #####
##########################################################

categories.by.country = data.individuals %>%
  select(Birth.Country, Category) %>% 
  group_by(Birth.Country, Category) %>% 
  summarise(number = n()) 

short.list.cats.by.country = categories.by.country[ categories.by.country$Birth.Country %in% list.of.top.countries , ]


ggplot (data = short.list.cats.by.country, aes(x = Birth.Country, y = number, fill = Category)) +
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.title.y =element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(), axis.ticks.x = element_blank()) +
  guides(fill=guide_legend(title= element_blank())) +
  scale_fill_brewer(type = "seq", palette = "BuPu") +
  ggtitle("Most Common Categories by Country, percentage")


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






