#---------------------PROJEKT KOD - WIZUALIZACJA DANYCH------------------------#
#Autorzy: Aleksandra Kacprzyk 114125, Sofiya Iadkouskaya 115079

#biblioteki
library(CAST)
library(sf)
library(readr)
library(dplyr)
library(tidyverse)
#install.packages("naniar")
library(naniar)
library(ggplot2)
library(tmap)
#install.packages("viridis")
library(viridis)
library(grid)
#install.packages("olsrr")
library(olsrr)
library(car)
#install.packages("lmtest")
library(lmtest)

#przygotowanie zbioru danych####################################################
##załadowanie zbioru i wstępna analiza
data <- read.csv(
  "C:/Users/Ola/Documents/sgh_master/sem1/wiz/projekt - nieruchomosci/Melbourne_housing_FULL.csv", 
  header = TRUE, sep = ",")
summary(data)
dim(data)
head(data)

##braki danych
any(is.na(data))
apply(data, 2, anyNA)
sum(is.na(data$Car)) 
sum(is.na(data$BuildingArea)) 
sum(is.na(data$YearBuilt)) 

##wykres z brakami danych
data  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()+
  scale_fill_viridis_d()

vis_miss(data)

##modyfikacja zbioru
data_final <- data %>%
  select(-c(Bedroom2, Bathroom, Car, Landsize, BuildingArea, Address, SellerG, Postcode, Propertycount))%>%
  mutate(YearInfo = ifelse(is.na(YearBuilt), 0, 1))

vis_miss(data_final)

##stworzenie osobnego zbioru z rokiem budowy
data_final_year <- data_final

##dalsza modyfikacja zbioru głownego
data_final <- data_final %>% 
  select(- YearBuilt)

##usunięcie braków danych dla obu zbiorów
data_final <- na.omit(data_final)
data_final_year <- na.omit(data_final_year)

##analiza obu powstałych zbiorów
summary(data_final)
dim(data_final)
head(data_final)

summary(data_final_year)
dim(data_final_year)
head(data_final_year)

#konwersja poszcególnych zmiennych
data_final$Distance <- as.numeric(data_final$Distance)
data_final$Date <- as.Date(data_final$Date, format = "%d/%m/%Y")

#Analiza poszczególnych zmiennych###############################################
##Histogram
###cena
plot_all_price <- ggplot(data_final, aes(x = Price))+
  geom_histogram(binwidth = 200000, fill = "purple", 
                 color = "darkviolet", alpha = 0.7)+
  labs(title = "Histogram cen",
       x = "Cena",
       y = "Ilość")+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

plot_5000000_price <- ggplot(data_final %>% filter(Price > 5000000), aes(x = Price))+
  geom_histogram(binwidth = 200000, fill = "purple", 
                 color = "darkviolet", alpha = 0.7)+
  labs(title = "Histogram cen dla większych od 5 Milionów",
       x = "Cena",
       y = "Ilość")+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(
    plot.background = element_rect(fill = "floralwhite", color = "gray88"),
    plot.title = element_text(hjust = 0.5, size = 9, face = "bold"))

plot_all_price +
  annotation_custom(
    grob = ggplotGrob(plot_5000000_price),
    xmin = 3000000, xmax = 11200000,
    ymin = 1000, ymax = 4000
  )

###dystans
ggplot(data_final, aes(x = Distance))+
  geom_histogram(binwidth = 2, fill = "purple", 
                 color = "darkviolet", alpha = 0.7)+
  scale_x_continuous(
    breaks = seq(0, max(data_final$Distance), by = 4),  
    labels = paste(seq(0, max(data_final$Distance), by = 4), "km"))+
  labs(title = "Histogram dystansu",
       x = "Dystans od centrum",
       y = "Ilość")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

###ilosc pokoi
plot_all_rooms <- ggplot(data_final, aes(x = Rooms))+
  geom_histogram(binwidth = 1, fill = "purple", 
                 color = "darkviolet", alpha = 0.7)+
  labs(title = "Histogram ilości pokoi",
       x = "Ilość pokoi",
       y = "Ilość")+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

plot_6_rooms <- ggplot(data_final %>% filter(Rooms > 6), aes(x = Rooms))+
  geom_histogram(binwidth = 1, fill = "purple", 
                 color = "darkviolet", alpha = 0.7)+
  labs(title = "Histogram ilości pokoi dla więcej niż 6",
       x = "Ilość pokoi",
       y = "Ilość")+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(
    plot.background = element_rect(fill = "floralwhite", color = "gray88"),
    plot.title = element_text(hjust = 0.5, size = 7, face = "bold"))

plot_all_rooms +
  annotation_custom(
    grob = ggplotGrob(plot_6_rooms),
    xmin = 6, xmax = 16,
    ymin = 2000, ymax = 9000
  )

#stworzenie nowych kategorii
data_final <- data_final %>%
  mutate(Rooms_cat = case_when(
    Rooms == 1 ~ "1",
    Rooms == 2 ~ "2",
    Rooms == 3 ~ "3",
    Rooms == 4 ~ "4",
    Rooms == 5 ~ "5",
    Rooms >= 6 ~ "6 lub więcej",
  ))

ggplot(data_final, aes(x = factor(Rooms_cat)))+
  geom_bar(fill = "purple", 
                 color = "darkviolet", alpha = 0.7)+
  labs(title = "Histogram ilości pokoi",
       x = "Ilość pokoi",
       y = "Ilość")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

##wykres słupkowy - rozkłady
###typy nieruchomości
ggplot(data_final, aes(x = Type, fill = Type)) +
  geom_bar() +
  scale_fill_viridis_d() +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.5, size = 3) +
  scale_x_discrete(
    labels = c("h" = "Dom, domek, willa, \nszeregowiec", 
               "t" = "Kamienica", 
               "u" = "Lokal, bliźniak"))+
  labs(title = "Rozkład typów nieruchomości", x = "Typ nieruchomości", y = "Liczba")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))+
  guides(fill = "none")

###Metoda sprzedaży nieruchomości
ggplot(data_final, aes(x = Method, fill = Method)) +
  geom_bar() +
  scale_fill_viridis_d() +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.5, size = 3) +
  scale_x_discrete(
    labels = c("S" = "nieruchomość \nsprzedana", 
               "PI" = "nieruchomość \nprzekazana",
               "SP" = "nieruchomość \nsprzedana \nwcześniej", 
               "VB" = "oferta \nsprzedającego", 
               "SA" = "sprzedane \npo aukcji"))+
  labs(title = "Rozkład metody sprzedaży nieruchomości", 
       x = "Metoda sprzedaży nieruchomości", y = "Liczba")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))+
  guides(fill = "none")

###Rozkład sprzedaży nieruchomości według daty
ggplot(data_final, aes(x = Date, fill = ..count..)) +
  geom_bar(width = 7) +
  scale_fill_viridis_c(option = "viridis") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") + 
  labs(title = "Rozkład sprzedaży nieruchomości według daty", 
       x = "Data sprzedaży nieruchomości", y = "Liczba", fill = "Liczba")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))+
  guides(fill = "none")

###Liczba nieruchomości według obszarów samorządowych
ggplot(data_final, aes(x = CouncilArea, fill = CouncilArea)) +
  geom_bar() +
  scale_fill_viridis_d()+
  labs(title = "Liczba nieruchomości według obszarów samorządowych", 
       x = "Obszary samorządowe", y = "Liczba") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))+
  guides(fill = "none")

###Liczba nieruchomości według regionów
ggplot(data_final, aes(x = Regionname, fill = Regionname)) +
  geom_bar() +
  scale_fill_viridis_d()+
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.1, size = 3) +
  labs(title = "Liczba nieruchomości według regionów", x = "Regiony", y = "Liczba") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))+
  guides(fill = "none")

##wykres kołowy i histogram dla zmiennej year
###wykres kołowy - ile nieruchomości ma podany rok
####modyfikacja zbioru
summary_data <- data_final %>%
  group_by(YearInfo) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

summary_data$YearInfo <- factor(summary_data$YearInfo, levels = c(0, 1), labels = c("nie", "tak"))

###wykres kołowy
piechart_year <- ggplot(summary_data, aes(x = "", y = percentage, fill = factor(YearInfo))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  theme_void() + 
  labs(fill = "Legenda", title = "Czy jest podany rok nieruchomości") +
  scale_fill_viridis_d()+
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3)+
  theme(plot.title = element_text(hjust = 0, size = 7, face = "bold"),
        plot.background = element_rect(fill = "floralwhite", color = "gray88"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

##histogram
hist_year <- ggplot(data_final_year %>% filter(YearBuilt > 1800), aes(x = YearBuilt))+
  geom_histogram(binwidth = 2, fill = "purple", 
                 color = "darkviolet", alpha = 0.7)+
  labs(title = "Histogram roku",
       x = "Rok (od 1800)",
       y = "Ilość")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

hist_year +
  annotation_custom(
    grob = ggplotGrob(piechart_year),
    xmin = 1800, xmax = 1950,
    ymin = 550, ymax = 1250
  )

#analiza 2 czynnikowa###########################################################
##Procentowy udział regionów w typach nieruchomości
ggplot(data_final, aes(x = Type, fill = Regionname)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d()+
  scale_x_discrete(
    labels = c("h" = "Dom, domek, willa, \nszeregowiec", 
               "t" = "Kamienica", 
               "u" = "Lokal, bliźniak"))+
  labs(title = "Rozkład typów nieruchomości", 
       x = "Typ nieruchomości", y = "Liczba")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

##Liczba nieruchomości w zależności od typu i regionu
ggplot(data_final, aes(x = Type, fill = Type)) +
  geom_bar(color = "black") +
  facet_wrap(~ Regionname) +
  scale_fill_viridis_d()+
  scale_x_discrete(
    labels = c("h" = "Dom, domek, willa, \nszeregowiec", 
               "t" = "Kamienica", 
               "u" = "Lokal, bliźniak"))+
  labs(title = "Liczba nieruchomości w zależności od typu i regionu",
       x = "Typ nieruchomości",
       y = "Liczba") +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.1, size = 3)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))+
  guides(fill = "none")

##Liczba nieruchomości w zależności od typu i metody sprzedaży
ggplot(data_final, aes(x = Type, fill = Method)) +
  geom_bar(position = "dodge", color = "black") +
  scale_x_discrete(
    labels = c("h" = "Dom, domek, willa, \nszeregowiec", 
               "t" = "Kamienica", 
               "u" = "Lokal, bliźniak"))+
  scale_fill_viridis_d(
    labels = c("S" = "nieruchomość \nsprzedana", 
               "PI" = "nieruchomość \nprzekazana",
               "SP" = "nieruchomość \nsprzedana \nwcześniej", 
               "VB" = "oferta \nsprzedającego", 
               "SA" = "sprzedane \npo aukcji"))+
  theme_minimal()+
  labs(title = "Liczba nieruchomości w zależności od typu i metody sprzedaży",
       x = "Typ nieruchomości",
       y = "Liczba",
       fill = "Metoda sprzedaży")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

#Analiza ceny###################################################################
##analiza 
###Średnia cena w czasie
avg_price_time <- data_final %>%
  group_by(Date) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE))

ggplot(avg_price_time, aes(x = Date, y = AvgPrice)) +
  geom_line(color = "purple", size = 1) +
  geom_smooth(color = "yellow")+
  labs(title = "Średnia cena nieruchomości w czasie",
       x = "Data",
       y = "Średnia cena") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

###Średnia cena w czasie dla ilosci pokoi
avg_price_time_Room <- data_final %>%
  group_by(Date, Rooms_cat) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE))

ggplot(avg_price_time_Room, aes(x = Date, y = AvgPrice, color = Rooms_cat)) +
  geom_line(size = 1) +
  scale_color_viridis_d()+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Średnia cena nieruchomości w czasie z zależności od ilości pokoi",
       x = "Data",
       y = "Średnia cena",
       color = "Ilość pokoi") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

###Średnia cena w czasie dla typu
avg_price_time_Type <- data_final %>%
  group_by(Date, Type) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE))

ggplot(avg_price_time_Type, aes(x = Date, y = AvgPrice, color = Type)) +
  geom_line(size = 1) +
  scale_color_viridis_d(
    labels = c("h" = "Dom, domek, willa, \nszeregowiec", 
               "t" = "Kamienica", 
               "u" = "Lokal, bliźniak"))+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Średnia cena nieruchomości w czasie z zależności od typu nieruchomości",
       x = "Data",
       y = "Średnia cena",
       color = "Typ nieruchomości") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

###Średnia cena w czasie dla regionu
avg_price_time_Region <- data_final %>%
  group_by(Date, Regionname) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE))

ggplot(avg_price_time_Region, aes(x = Date, y = AvgPrice, color = Regionname)) +
  geom_line(size = 1) +
  scale_color_viridis_d()+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Średnia cena nieruchomości w czasie z zależności od regionu",
       x = "Data",
       y = "Średnia cena",
       color = "Region") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

##analiza ceny z innymi zmiennymi
#### Wykres pudełkowy cena a typ
ggplot(data_final, aes(x = Type, y = Price)) +
  geom_boxplot(fill = "violet", color = "purple") +
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(
    labels = c("h" = "Dom, domek, willa, \nszeregowiec", 
               "t" = "Kamienica", 
               "u" = "Lokal, bliźniak"))+
  labs(title = "Cena w zależności od typu nieruchomości", 
       x = "Typ nieruchomości", y = "Cena")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#### Wykres pudełkowy cena a region
ggplot(data_final, aes(x = Regionname, y = Price)) +
  geom_boxplot(fill = "violet", color = "purple") + 
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Rozkład cen nieruchomości w zależności od regionu", 
       x = "Region", y = "Cena") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#### Wykres pudełkowy cena a ilosc pokoi 
ggplot(data_final, aes(x = factor(Rooms_cat), y = Price)) +
  geom_boxplot(fill = "violet", color = "purple") +
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Cena w zależności od ilosci pokoi", 
       x = "Ilość pokoi", y = "Cena")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#### Wykres pudełkowy cena a obszary samorządowe
ggplot(data_final, aes(x = CouncilArea, y = Price)) +
  scale_y_continuous(labels = scales::comma)+
  geom_boxplot(fill = "violet", color = "purple") +
  labs(title = "Cena w zależności od obszarów samorządowych", 
       x = "Typ nieruchomości", y = "Cena")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

###Histogram cena a regiony
ggplot(data_final, aes(x = Price))+
  geom_histogram(binwidth = 200000, fill = "purple", 
                 color = "darkviolet", alpha = 0.7)+
  facet_wrap(~ Regionname) +
  labs(title = "Histogram cen z podziałem na regiony",
       x = "Cena",
       y = "Ilość")+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

###Zależność ceny od odległości od centrum
ggplot(data_final, aes(x = Distance, y = Price)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm", color = "yellow")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(
    breaks = seq(0, max(data_final$Distance), by = 2),  
    labels = paste(seq(0, max(data_final$Distance), by = 2), "km")
  ) + 
  labs(title = "Zależność ceny od odległości od centrum",
       x = "Odległość od centrum (km)",
       y = "Cena nieruchomości") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

###Cena nieruchomości w zależności od odległości od centrum z podziałem na typy
ggplot(data_final, aes(x = Distance, y = Price, fill = Type)) +
  geom_point(alpha = 0.6, size = 2, shape = 21) +
  labs(title = "Cena nieruchomości w zależności od odległości od centrum z podziałem na typy",
       x = "Odległość od centrum (km)",
       y = "Cena nieruchomości",
       fill = "Typ nieruchomości") +
  scale_y_continuous(labels = scales::comma)+
  scale_fill_viridis_d(
    labels = c("h" = "Dom, domek, willa, \nszeregowiec", 
               "t" = "Kamienica", 
               "u" = "Lokal, bliźniak"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))+
  facet_wrap(~ Type, scales = "free_y")

###Cena nieruchomości w zależności od odległości od centrum z podziałem na ilość pokoi
ggplot(data_final, aes(x = Distance, y = Price, fill = factor(Rooms_cat)))+
  geom_point(alpha = 0.6, size = 2, shape = 21) +
  labs(title = "Cena nieruchomości w zależności od odległości od centrum z podziałem na ilość pokoi",
       x = "Odległość od centrum (km)",
       y = "Cena nieruchomości",
       fill = "Ilość pokoi") +
  scale_y_continuous(labels = scales::comma)+
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))+
  facet_wrap(~Rooms_cat , scales = "free_y")

###Cena a rok budowy
graph_year <- ggplot(data_final_year %>% filter(YearBuilt > 1800), 
                     aes(x = YearBuilt, y = Price)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "yellow")+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Zależność ceny od roku budowy",
       x = "Rok budowy",
       y = "Cena nieruchomości") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

graph_year +
  annotation_custom(
    grob = ggplotGrob(piechart_year),
    xmin = 1810, xmax = 1875,
    ymin = 5000000, ymax = 9500000
  )

#MAPA###########################################################################
#przygotowanie danych do mapy
data_clustered <- data_final %>%
  group_by(Longtitude, Lattitude) %>%
  summarize(Liczba = n(), .groups = "drop")
data_clustered <- st_as_sf(data_clustered, coords = c("Longtitude", "Lattitude"), crs = 4326)

melbourne <- read_sf(
  "C:/Users/Ola/Documents/sgh_master/sem1/wiz/projekt - nieruchomosci/street-names/street-names.shp"
) %>%
  st_transform(4326) 
plot(melbourne)


#biblioteka
#install.packages("tmap")
library(tmap)

tmap_mode("plot")

map_melbourne_clustered <- tm_shape(melbourne) + 
  tm_lines(col = "gray60", lwd = 0.5) +
  tm_shape(data_clustered)+ 
  tm_bubbles(size = "Liczba", 
             col = "Liczba", 
             palette = "viridis", 
             border.col = "white",  
             alpha = 0.5,         
             scale = 2)  +
  tm_layout(legend.outside = TRUE, 
            legend.outside.size = 0.3,  
            legend.title.size = 1.2,    
            legend.text.size = 0.8,     
            main.title = "Gęstość lokalizacji zebranych we zbiorze", 
            main.title.size = 1.2)

map_melbourne_clustered

################################################################################

# Podział na zbiór treningowy i testowy
set.seed(17)
test_prop <- 0.25
test.set.index <- (runif(nrow(data_final)) < test_prop)
data_final.test <- data_final[test.set.index, ]
data_final.train <- data_final[!test.set.index, ]


#Regresja#######################################################################
reg <- lm(Price ~ factor(Type) + factor(Rooms_cat) + Distance + factor(Regionname),
          data = data_final.train)
summary(reg)
coef(reg)

##Współliniowość zmiennych
vif(reg)

##Test reset
resettest(reg)

##Test niezależności reszt
dwtest(reg, data = data_final.train)

##Test na heteroskedastyczność
bptest(reg, data = data_final.train)



#regresja - zmienna Price jako zmienna klasowa
##zmiana ceny na zmienna klasowa

#biblioteka
#install.packages("nnet")
library(nnet)
install.packages("pscl")
library(pscl)

data_final$Price_group <- cut(data_final$Price, 
                                    breaks = c(85000, 657000, 1089746, 1335000, 11200000), 
                                    labels = c("Niska", "Średnia", "Wysoka", "Bardzo Wysoka"),
                                    include.lowest = TRUE)
set.seed(17)
test_prop <- 0.25
test.set.index <- (runif(nrow(data_final)) < test_prop)
data_final.test <- data_final[test.set.index, ]
data_final.train <- data_final[!test.set.index, ]

reg_class <- multinom(Price_group ~ factor(Type) + factor(Rooms_cat) + Distance + factor(Regionname),
          data = data_final.train)
summary(reg_class)

pscl::pR2(reg_class)

#Drzewo klasyfikacyjne##########################################################

#biblioteki
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(MASS)

tree <- rpart(Price_group ~ factor(Type) + factor(Rooms_cat) + Distance + factor(Regionname),
              data = data_final.train,
              method = "class")
rpart.plot(tree, under = FALSE, tweak = 0.55, fallen.leaves = TRUE, box.palette = "Purples", cex = 1)

tree_deeper <- rpart(Price_group ~ factor(Type) + factor(Rooms_cat) + Distance + factor(Regionname),
                     data = data_final.train,
                     method = "class",
                      control = list(cp = 0.005))
rpart.plot(tree_deeper, under = FALSE,tweak = 0.55, fallen.leaves = TRUE, box.palette = "Purples", cex = 0.9)

## Wpływ zmiennych
tree$variable.importance
tree_deeper$variable.importance

#Las losowy#####################################################################
rf <- randomForest(Price_group ~ Type + Rooms_cat + Distance + Regionname,
                   data = data_final.train)
rf

## Wpływ zmiennych
varImpPlot(rf)
rf$importance

#Weryfikacja jakości############################################################
## Macierz pomyłek

CM <- list()

### Regresje
CM[["reg"]] <- table(ifelse(predict(reg, new = data_final.test, type = "response") > 0.5, 1, 0), data_final.test$Price)
CM[["reg_class"]] <- table(predict(reg_class, newdata = data_final.test, type = "class"),data_final.test$Price_group)
### Drzewa
CM[["tree"]] <- table(predict(tree, new = data_final.test, type = "class"), data_final.test$Price_group)
CM[["tree_deeper"]] <- table(predict(tree_deeper, new = data_final.test, type = "class"), data_final.test$Price_group)
### Las
CM[["rf"]] <- table(predict(rf, new = data_final.test, type = "class"), data_final.test$Price_group)

EvaluateModel <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy 
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}


sapply(CM, EvaluateModel)

# Weryfikacja jakości klasyfikacji - ROC
## Prognoza w postaci prawdopodobieństwa, zamiast kategorii
preds <- list()

### Regresje
preds[["reg_class"]] <- as.vector(predict(reg_class, newdata = data_final.test, type = "class"))
### Drzewa
preds[["tree"]] <- as.vector(predict(tree, newdata = data_final.test)[, 2])
preds[["tree_glebsze"]] <- as.vector(predict(tree_deeper, newdata = data_final.test)[, 2])
### Las
preds[["rf"]] <- as.vector(predict(rf, newdata = data_final.test, type = "prob")[, 2])

#biblioteka
library(pROC)

#chcemy 4 wykresy obok siebie
par(mfrow = c(2, 2), mar = c(4, 4, 2, 8))

#by ujednolicić kolorstycznie
viridis_colors <- viridis(length(preds))


for (class_label in levels(data_final.test$Price_group)) {
  binary_truth <- ifelse(data_final.test$Price_group == class_label, 1, 0)
  #jeden wykres dla kazdej klasy - wiec zamiana na zmienna binarna
  plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
       xlab = "Wskaźnik wyników fałszywie pozytywnych", ylab = "Wskaźnik wyników prawdziwie pozytywnych", 
       main = paste("Krzywa ROC dla grupy cenowej:", class_label))

  for (i in 1:length(preds)) {
    if (is.numeric(preds[[i]])) {
      pred <- prediction(preds[[i]], binary_truth)
    } else {
      pred <- prediction(as.numeric(preds[[i]] == class_label), binary_truth)
    }
    #Liczenie ROC
    perf <- performance(pred, "tpr", "fpr")
    #Rysowanie wykresów
    plot(perf, col = viridis_colors[i], lwd = 2, add = TRUE)
  }

  abline(coef = c(0, 1), lty = 2, lwd = 0.5)
}

#Legenda
legend(x = 1.1, y = 1, 
       legend = names(preds), 
       col = viridis_colors, 
       lty = 1, 
       lwd = 2, 
       cex = 0.8,  
       box.lty = 0, 
       xpd = TRUE)

par(mfrow = c(1, 1))

#AUC 
for (i in 1:length(preds)) {
  for (class_label in levels(data_final.test$Price_group)) {
    binary_truth <- ifelse(data_final.test$Price_group == class_label, 1, 0)
    
    if (is.numeric(preds[[i]])) {
      roc_obj <- pROC::roc(binary_truth, preds[[i]])
      auc_value <- pROC::auc(roc_obj)
    } else {
      pred <- prediction(as.numeric(preds[[i]] == class_label), binary_truth)
      auc_value <- performance(pred, "auc")@y.values[[1]]
    }
    cat(names(preds)[i], "-", class_label, ": AUC = ", auc_value, "\n")
  }
}
