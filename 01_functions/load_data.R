library(janitor)
library(tidyverse)
library(lubridate)
library(rsample)

data <- read.csv('00_data/Sberbank Dataset.csv') %>%
  clean_names()

macro <- read.csv('00_data/Macro.csv') %>%
  mutate(timestamp = ymd(timestamp))


# features we want to focus on
features <- c("timestamp","price_doc","full_sq", "floor", # "max_floor",
              "product_type", "sub_area", "num_room",
              "kindergarten_km", "green_zone_km", "school_km", "metro_min_avto", 
              "public_healthcare_km", "industrial_km", "metro_min_walk", "public_transport_station_km",
              "railroad_km", "swim_pool_km", "park_km", "fitness_km", "radiation_km", "cemetery_km",
              "big_road1_km", "theater_km", "market_shop_km", "hospice_morgue_km","metro_km_avto", 
              "metro_km_walk", "water_treatment_km", "incineration_km", "railroad_station_walk_km",
              "railroad_station_walk_min", "railroad_station_avto_km", "railroad_station_avto_min",
              "school_education_centers_raion", "school_education_centers_top_20_raion",
              "healthcare_centers_raion", "university_top_20_raion", "sport_objects_raion",
              "additional_education_raion", "culture_objects_top_25_raion", "shopping_centers_raion",
              "office_raion", "kremlin_km", "big_road2_km", "bus_terminal_avto_km", "market_shop_km",
              "basketball_km", "university_km", "mosque_km", "big_church_km", "church_synagogue_km",
              "museum_km","raion_popul", "sport_objects_raion", "additional_education_raion", 
              "culture_objects_top_25", "shopping_centers_raion",
              "full_all", "big_road1_km", "zd_vokzaly_avto_km", "big_market_km", "market_shop_km",
              "stadium_km", "university_km")


df <- as_tibble(data %>% select(features))


df <- df %>% 
  filter(full_sq>20 & full_sq < 200) %>%
  mutate(timestamp = as_date(as.POSIXct(timestamp,format="%m/%d/%Y"),tz=NULL)) %>%
  left_join(macro %>% select(timestamp, eurrub)) %>% 
  mutate(price_eur = price_doc / eurrub) %>% 
  mutate(eur_sqm = round(price_eur / full_sq ))
  
#collapse sub_area
Zelenogradsky <-c("Krjukovo","Matushkino","Savelki","Silino","Staroe Krjukovo")
Novomoskovsky <-c("Poselenie Desjonovskoe","Poselenie Filimonkovskoe","Poselenie Kokoshkino","Poselenie Marushkinskoe","Poselenie Moskovskij","Poselenie Mosrentgen","Poselenie Rjazanovskoe","Poselenie Shherbinka","Poselenie Sosenskoe","Poselenie Vnukovskoe","Poselenie Voskresenskoe")
Troitsky <-c("Poselenie Kievskij","Poselenie Klenovskoe","Poselenie Krasnopahorskoe","Poselenie Mihajlovo-Jarcevskoe","Poselenie Novofedorovskoe","Poselenie Pervomajskoe","Poselenie Rogovskoe","Poselenie Shhapovskoe","Poselenie Voronovskoe","Troickij okrug")
Northern <-c("Ajeroport","Begovoe","Beskudnikovskoe","Dmitrovskoe","Golovinskoe","Horoshevskoe","Hovrino","Koptevo","Levoberezhnoe","Molzhaninovskoe","Savelovskoe","Sokol","Timirjazevskoe","Vojkovskoe","Vostochnoe Degunino","Zapadnoe Degunino")
Southwest <-c("Akademicheskoe","Cheremushki","Gagarinskoe","Jasenevo","Juzhnoe Butovo","Kon'kovo","Kotlovka","Lomonosovskoe","Obruchevskoe","Severnoe Butovo","Teplyj Stan","Zjuzino")
Northeast <-c("Alekseevskoe","Altuf'evskoe","Babushkinskoe","Bibirevo","Butyrskoe","Jaroslavskoe","Juzhnoe Medvedkovo","Lianozovo","Losinoostrovskoe","Mar'ina Roshha","Marfino","Ostankinskoe","Otradnoe","Rostokino","Severnoe","Severnoe Medvedkovo","Sviblovo")
Central <-c("Arbat","Basmannoe","Hamovniki","Jakimanka","Krasnosel'skoe","Meshhanskoe","Presnenskoe","Taganskoe","Tverskoe","Zamoskvorech'e")
Southern <-c("Birjulevo Vostochnoe","Birjulevo Zapadnoe","Brateevo","Caricyno","Chertanovo Central'noe","Chertanovo Juzhnoe","Chertanovo Severnoe","Danilovskoe","Donskoe","Moskvorech'e-Saburovo","Nagatino-Sadovniki","Nagatinskij Zaton","Nagornoe","Orehovo-Borisovo Juzhnoe","Orehovo-Borisovo Severnoe","Zjablikovo")
Eastern <-c("Bogorodskoe","Gol'janovo","Ivanovskoe","Izmajlovo","Kosino-Uhtomskoe","Metrogorodok","Novogireevo","Novokosino","Perovo","Preobrazhenskoe","Severnoe Izmajlovo","Sokol'niki","Sokolinaja Gora","Veshnjaki","Vostochnoe","Vostochnoe Izmajlovo")
Western <-c("Dorogomilovo","Filevskij Park","Fili Davydkovo","Krylatskoe","Kuncevo","Mozhajskoe","Novo-Peredelkino","Ochakovo-Matveevskoe","Prospekt Vernadskogo","Ramenki","Solncevo","Troparevo-Nikulino","Vnukovo")
Northwest <-c("Horoshevo-Mnevniki","Juzhnoe Tushino","Kurkino","Mitino","Pokrovskoe Streshnevo","Severnoe Tushino","Shhukino","Strogino")
Southeast <-c("Juzhnoportovoe","Kapotnja","Kuz'minki","Lefortovo","Ljublino","Mar'ino","Nekrasovka","Nizhegorodskoe","Pechatniki","Rjazanskij","Tekstil'shhiki","Vyhino-Zhulebino")

df$neighborhood[df$sub_area %in% Zelenogradsky] <- "Zelenogradsky"
df$neighborhood[df$sub_area %in% Novomoskovsky] <- "Novomoskovsky"
df$neighborhood[df$sub_area %in% Troitsky] <- "Troitsky"
df$neighborhood[df$sub_area %in% Northern] <- "Northern"
df$neighborhood[df$sub_area %in% Southwest] <- "Southwest"
df$neighborhood[df$sub_area %in% Northeast] <- "Northeast"
df$neighborhood[df$sub_area %in% Central] <- "Central"
df$neighborhood[df$sub_area %in% Southern] <- "Southern"
df$neighborhood[df$sub_area %in% Eastern] <- "Eastern"
df$neighborhood[df$sub_area %in% Western] <- "Western"
df$neighborhood[df$sub_area %in% Northwest] <- "Northwest"
df$neighborhood[df$sub_area %in% Southeast] <- "Southeast"

# Train/Test split ----
df_ml <- df %>% select(-price_doc, -price_eur, -eurrub, -timestamp)
set.seed(11)
split <- initial_split(df_ml, prop = 0.8, strata = eur_sqm)
train_data <- split %>% training()
test_data <- split %>% testing()

