getwd()
setwd("C:/Users/dejan.bulaja/Desktop/R Programming/Housing project/")
getwd()


library(DataExplorer)
library(tidyverse)
library(scales)
library(corrplot)
library(plotly)
library(highcharter)
library(tidyquant)
library(ggplot2)

# Load Data ----
source('01_functions/load_data.R')



# Price / Full Sq.
plot_price_fullsq <- df %>%
  ggplot(aes(x=full_sq, y=price_eur)) + geom_point(col="black") +
  geom_smooth(se=F) +
  scale_y_continuous(labels = dollar_format( suffix="\u20ac", prefix ="", big.mark = ",")) +
  labs(title= "Full Sq./ Price",
       x= "Full Sq.",
       y= "Price") + theme_tq()

# Mean Price / neighborhood

plot_avgprice_neighborhood <- df %>% 
  group_by(neighborhood) %>% 
  summarize(across(price_eur, mean)) %>% 
  plot_ly(
    x = ~neighborhood,
    y = ~price_eur,
    name = "Average price per neighborhood",
    type = "bar"
  ) %>% layout(title = "Average price per neighborhood",
               xaxis = list(title = "Neighborhood"),
               yaxis = list(title = "Average price", 
                            tickformat = "\u20ac",
                            ticksuffix = "\u20ac"))

# Mean Price/Sqm  / Neighborhood

plot_avgeursqm_neighborhood <- df %>% 
  group_by(neighborhood) %>% 
  summarize(across(eur_sqm, mean)) %>%
  mutate(eur_sqm=round(eur_sqm)) %>%
  plot_ly(
    x = ~neighborhood,
    y = ~eur_sqm,
    name = "Average price per neighborhood",
    type = "bar"
  ) %>% layout(title = "Average price/sqm per neighborhood",
               xaxis = list(title = "Neighborhood"),
               yaxis = list(title = "Average price",
                            tickformat = "\u20ac",
                            ticksuffix = "\u20ac"))

# Average price per product type

plot_avgprice_prodtype <- df %>% 
  group_by(product_type) %>% 
  summarize(across(price_eur, mean)) %>% 
  mutate(price_eur=round(price_eur)) %>%
  plot_ly(
    x = ~product_type,
    y = ~price_eur,
    name = "Average price per product type",
    type = "bar"
  ) %>% layout(title = "Average price per product type",
               xaxis = list(title = "Product Type"),
               yaxis = list(title = "Average price",
                            tickformat = "\u20ac",
                            ticksuffix = "\u20ac"))

# Average price/sqm per product type

plot_avgeursqm_prodtype <- df %>% 
  group_by(product_type) %>% 
  summarize(across(eur_sqm, mean)) %>% 
  plot_ly(
    x = ~product_type,
    y = ~eur_sqm,
    name = "Average price per product type",
    type = "bar"
  ) %>% layout(title = "Average price/sqm per product type",
               xaxis = list(title = "Product Type"),
               yaxis = list(title = "Average price"))

# Room Count Distribution

plot_roomcount <- df %>% select("num_room") %>% 
  group_by(num_room) %>% 
  tally() %>%
  plot_ly(x = ~num_room,
          y = ~n,
          type = "bar",
          name = "Room Count") %>% 
  layout(title = "Room count distribution",
         xaxis = list(title = "Number of Rooms"),
         yaxis = list(title = ""))


# Average Number of Rooms per neighborhood
plot_avgnumrooms_neighborhood <- df %>%
  group_by(neighborhood) %>%
  summarize(across(full_sq,mean)) %>%
  mutate(full_sq = round(full_sq)) %>%
  plot_ly(
    x = ~neighborhood,
    y = ~full_sq,
    name = "Average size per neighborhood",
    type = "bar"
  ) %>% layout(title = "Average size per neighborhood",
               xaxis = list(title = "Neighborhood"),
               yaxis = list(title = "Average size in sq/m"))


# Save Image ----

save.image('env/eda_data.RData')
