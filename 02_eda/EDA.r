library(tidyverse)
library(scales)
library(corrplot)
library(plotly)
library(highcharter)
library(tidyquant)
library(ggplot2)
library(leaflet) # interactive maps
library(glue)
library(reshape2)

# Load Data ----
source('01_functions/load_data.R')


# Price / Full Sq.
plot_price_fullsq <- ggplotly(df %>% 
                                mutate(price_eur = round(price_eur)) %>%
  ggplot(aes(x=full_sq, y=price_eur)) + geom_point(col="black") +
  geom_smooth(se=F) +
  scale_y_continuous(labels = dollar_format(suffix="\u20ac", prefix ="", big.mark = ",")) +
  labs(title= "Full Sq./ Price",
       x= "Full Sq.",
       y= "Price") + theme_tq())

# Mean Price / neighborhood

plot_avgprice_neighborhood <- df %>% 
  group_by(neighborhood) %>% 
  summarise(across(price_eur, mean)) %>% 
  plot_ly(
    x = ~neighborhood,
    y = ~round(price_eur),
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
  summarise(across(eur_sqm, mean)) %>%
  mutate(eur_sqm=round(eur_sqm)) %>%
  plot_ly(
    x = ~neighborhood,
    y = ~eur_sqm,
    name = "Average price per neighborhood",
    type = "bar"
  ) %>% layout(title = "Average price/m2 per neighborhood",
               xaxis = list(title = "Neighborhood"),
               yaxis = list(title = "Average price",
                            tickformat = "\u20ac",
                            ticksuffix = "\u20ac"))

# Average price per product type

plot_avgprice_prodtype <- df %>% 
  group_by(product_type) %>% 
  summarise(across(price_eur, mean)) %>% 
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
  summarise(across(eur_sqm, mean)) %>% 
  plot_ly(
    x = ~product_type,
    y = ~round(eur_sqm),
    name = "Average price per product type",
    type = "bar"
  ) %>% layout(title = "Average price/m2 per product type",
               xaxis = list(title = "Product Type"),
               yaxis = list(title = "Average price",
                            tickformat = "\u20ac",
                            ticksuffix = "\u20ac/m2"))

# Room Count Distribution

plot_roomcount <- ggplotly(df %>% select("num_room") %>% 
  group_by(num_room) %>% 
  tally() %>%
  plot_ly(x = ~num_room,
          y = ~n,
          type = "bar",
          name = "Room Count") %>% 
  layout(title = "Room count distribution",
         xaxis = list(title = "Number of Rooms"),
         yaxis = list(title = ""))
)

# Average size per neighborhood
plot_avgfullsq_neighborhood <- df %>%
  group_by(neighborhood) %>%
  summarise(across(full_sq,mean)) %>%
  mutate(full_sq = round(full_sq)) %>%
  plot_ly(
    x = ~neighborhood,
    y = ~full_sq,
    name = "Average size per neighborhood",
    type = "bar"
  ) %>% layout(title = "Average size per neighborhood",
               xaxis = list(title = "Neighborhood"),
               yaxis = list(title = "Average size in m2"))

# Room Count v Price

plot_roomcount_price <- ggplotly(df %>% 
                            ggplot(aes(num_room,price_eur)) +
                            geom_boxplot(aes(group=num_room), fill="#0099FF") +
                            scale_y_continuous(labels = dollar_format(suffix="\u20ac", prefix ="", big.mark = ",")) +
                            labs(title = "Price / Number of Rooms",
                                 x = "Number of rooms",
                                 y = "Price"))





# Moscow Map
coordinates <- coordinates %>% mutate(price_eur= round(price_eur))
plot_moscow_map <- leaflet(data = coordinates) %>%
                   addTiles() %>%
                   addMarkers(coordinates$X, 
                             coordinates$Y,
                             clusterOptions = markerClusterOptions(),
                             label = str_glue("Price: {coordinates$price_eur}, ",
                                              "Price/m2: {coordinates$eur_sqm}, ",
                                              " Neighborhood: {coordinates$neighborhood}, ",
                                              " Sq ft.: {coordinates$full_sq}"))



# Correlation

library(funModeling)
library(GGally)

top10_correlated <- df %>%
  select(-timestamp,-product_type,-sub_area,-culture_objects_top_25,-neighborhood,
         -price_eur, -price_doc) %>%
  drop_na() %>% correlation_table("eur_sqm") %>% top_n(11) %>% select(Variable)


top10_correlated$Variable

cormat <- round(cor(df %>% select(top10_correlated$Variable)),2)

melted_cormat <- melt(cormat)
head(melted_cormat)

plot_top10_correlations <- ggplotly(ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() + 
  labs(x="",
       y=""))

# Save Image ----
save.image('03_env/eda_data.RData')
