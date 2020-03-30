library(tidyverse)
library(ggplot2)
library(readr)
library(RColorBrewer)

full_data <- read_csv("full_data.csv")

march_29 <- full_data %>% filter(date == "2020-03-29") %>% arrange(desc(total_cases)) %>% select(location,total_cases, total_deaths)

corona_worldwide <-march_29[1,]

march_29_countries <- march_29[-1, ]
march_29_countries <- march_29_countries %>% mutate(total_cases_discrete = cut(march_29_countries$total_cases, 
                                                                               breaks=c(-Inf,0,500, 1000, 10000,100000, Inf), 
                                                                               labels=c("0","<500", "<1000","<10,000","<100,000", ">100,000")))

world <- map_data("world")
str(world)

world_corona <- world %>% left_join(march_29_countries, by = c("region" = "location"))  %>% arrange(desc(total_cases))

world_regions_corona <-world_corona %>% select(region, total_cases, total_deaths) %>% distinct()
NA_sum <- sum(is.na(world_regions_corona$total_cases))
NA_regions <- world_regions_corona%>% filter(is.na(world_regions_corona$total_cases))


USA_total_cases <- march_29_countries%>% filter(location=="United States") %>% select(total_cases)
USA_total_deaths <- march_29_countries%>% filter(location=="United States") %>% select(total_deaths)

NA_regions$total_cases <- ifelse(NA_regions$region == "USA", USA_total_cases, NA_regions$total_cases)
NA_regions$total_deaths <- ifelse(NA_regions$region == "USA", USA_total_deaths, NA_regions$total_deaths)

UK_total_cases <- march_29_countries%>% filter(location=="United Kingdom") %>% select(total_cases)
UK_total_deaths <- march_29_countries%>% filter(location=="United Kingdom") %>% select(total_deaths)

NA_regions$total_cases <- ifelse(NA_regions$region == "UK", UK_total_cases, NA_regions$total_cases)
NA_regions$total_deaths <- ifelse(NA_regions$region == "UK", UK_total_deaths, NA_regions$total_deaths)

NA_regions$total_cases <- ifelse(is.na(NA_regions$total_cases), 0, NA_regions$total_cases)
NA_regions$total_deaths <- ifelse(is.na(NA_regions$total_deaths), 0, NA_regions$total_deaths)

#_____________

world_corona_tidied <- world_corona[complete.cases(world_corona$total_cases,world_corona$total_cases ), ] 
NA_regions_full_data <- world %>% inner_join(NA_regions, by = "region")
NA_regions_full_data$total_cases <- as.numeric(NA_regions_full_data$total_cases)
NA_regions_full_data <- NA_regions_full_data %>% mutate(total_cases_discrete = cut(NA_regions_full_data$total_cases, 
                                                                  breaks=c(-Inf,0,500, 1000, 10000,100000, Inf), 
                                                                  labels=c("0","<500", "<1000","<10,000","<100,000", ">100,000")))
world_corona_tidied<- world_corona_tidied %>% rbind(NA_regions_full_data)

#####################################3
ggplot(world_corona_tidied, aes(x = long, y = lat, group = group, fill =total_cases_discrete )) +
  geom_polygon(col = "#000000") +
  scale_fill_brewer(palette = "Reds")+
  theme_void() 
  
