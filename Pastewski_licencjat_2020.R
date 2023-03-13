
# Daniel Pastewski
#Prognoznowanie popytu na pracę z wykorzystaniem hierachicznych szeregów czasowych
#Licencjat 2020


#Instalacja pakietu Thief, reszte miałem już zainstalowaną

install.packages('thief', dependencies = TRUE)


#Pakiety wykorzystywane podczas pracy

library(fst)
library(httr)
library(jsonlite)
library(tidyverse)
library(data.table)
library(ggplot2)
library(tidyverse)
library(graphics)
library(XML)
library(thief)
library(xts)
library(lubridate)


#Pozyskiwanie danych i zamiana na odpowiednie formaty.

doc_18 <- xmlParse("zestawieniepropozycji2018.xml") 
Node_18 <- xmlRoot(doc_18)
data_18_bis <- xmlSApply(Node_18,function(x) xmlSApply(x, xmlValue))
data_18_frame <- data.frame(t(data_18_bis),row.names=NULL)

doc_19 <- xmlParse("zestawieniepropozycji2019.xml")
Node_19 <- xmlRoot(doc_19)
data_19_bis <- xmlSApply(Node_19,function(x) xmlSApply(x, xmlValue))
data_19_frame <- data.frame(t(data_19_bis),row.names=NULL)


#Zapis i odczyt które przyśpieszały prace

saveRDS(data_19_frame, file = "data_19_frame.rds")
saveRDS(data_18_frame, file = "data_18_frame.rds")
xml_parent()

data_10_17 <- read.fst("dane_10/cbop2010_2017.fst")

data_18 <- readRDS("data_18_frame.rds")

data_19 <- readRDS("data_19_frame.rds")


#Wybór odpowiednich kolumn, uzgadnianie formatu i łączenie zbioróW

data_10_17_v1 <- as.data.table(data_10_17[,c(8,9,16)])
data_18_v1 <- as.data.table(data_18[,c(5,6,13)])
data_19_v1 <- as.data.table(data_19[,c(5,6,13)])

str(data_19_v1)
str(data_18_v1)
str(data_10_17_v1)

data_18_v1$data_przyj_oferty <- as.Date(data_18_v1$data_przyj_oferty, format = "%Y-%m-%d")
data_18_v1$kod_woj <- as.character(data_18_v1$kod_woj)

data_19_v1$data_przyj_oferty <- as.Date(data_19_v1$data_przyj_oferty, format = "%Y-%m-%d")
data_19_v1$kod_woj <- as.character(data_19_v1$kod_woj)

data_10_18 <- rbind(data_10_17_v1,data_18_v1)

data_10_19 <- rbind(data_10_18,data_19_v1)

data_11_19 <- data_10_19[data_przyj_oferty > as.Date("2010-12-31")]


# Zaimportownaie i dostosowanie pliku z kodami i nazwami zawodów z KZiS

tabele_zawody_x <- readRDS("tabele_zawody.rds") 
tabele_zawody <- as.data.table(tabele_zawody_x[,2:3])
names(tabele_zawody) <- c("kod_zawodu","nazwa_zawodu")

tabele_zawody[,1] <- as.factor(unlist(tabele_zawody[,1]))


# Dodanie do głównego zbioru nazw zawodu z KZiS

data_11_19_v1 <- merge(data_11_19,tabele_zawody) 

data_11_19_v1 <- data_11_19_v1[kod_zawodu != "NA"]


# Dodanie nazw województw do kodów 

kody_wojew <- data.frame(kod_woj = c("02","04","06","08","10","12","14","16","18","20","22","24","26","28","30","32"),
                         nazw_woj = c("Dolnośląskie","Kujawsko-Pomorskie","Lubelskie","Lubuskie","Łódzkie","Małopolskie", "Mazowieckie",
                                      "Opolskie","Podkarpackie","Podlaskie","Pomorskie","Śląskie","Świętokrzyskie","Warmiśko-Mazurskie",
                                      "Wielkopolskie","Zachodniopomorskie"))

data_11_19_v2 <- merge(data_11_19_v1,kody_wojew, by.x = "kod_woj", by.y = "kod_woj", all.x = T)


# Sprawdzenie jakie zawody są najpopularniejsze w polsce i województwie wielkopolskim i porównanie uzyskanych wartości

ilosc_zaw_wp <- data_11_19_v2[kod_woj == "30",.(.N,kod_zawodu), by = nazwa_zawodu] %>% distinct()

ilosc_zaw_wp <- ilosc_zaw_wp[order(-N)]

ilosc_zaw <- data_11_19_v2[,.(.N,kod_zawodu), by = nazwa_zawodu] %>% distinct()

ilosc_zaw <- ilosc_zaw[order(-N)]

kody_wielkopolska <- merge(ilosc_zaw[,1:2],ilosc_zaw_wp[,1:2], by.x = "nazwa_zawodu", by.y = "nazwa_zawodu")
kody_wielkopolska <- kody_wielkopolska[, procent := (N.y/N.x)]
kody_wielkopolska <- kody_wielkopolska[order(-N.y)]


# Wizualizacja miesięcznej liczby ofert na przestzreni lat

zawody_czas <- data_11_19_v2[,.(nazwa_zawodu,data_przyj_oferty)]

zawody_month <- zawody_czas[,czas := as.Date(paste(as.character(format(zawody_czas$data_przyj_oferty, "%Y-%m")),1, sep = "-"))]

ilosc_czas_m <- zawody_month[,.N,by = czas]

ilosc_czas_m <- ilosc_czas_m[order(czas)]

ggplot(ilosc_czas_m, aes(czas, N)) +
  geom_col(fill = "white", colour = "black") +
  xlab("Czas") +
  ylab("Liczba ofert") +
  ggtitle("Miesięczna liczba ofert pracy")


# Agregacja danych do poziomu miesiąca, zmiana formatu na time series

zawody_month <- data_11_19_v2[,czas := as.Date(paste(as.character(format(data_11_19_v2$data_przyj_oferty, "%Y-%m")),1, sep = "-"))]

ilosc_czas_m <- zawody_month[,.N,by = czas]

ilosc_czas_m <- ilosc_czas_m[order(czas)]

dane_m <- ilosc_czas_m[,2]
names(dane_m) <- "Liczba_ofert"


ts_dane_m <- ts(dane_m, start=c(2011, 1), frequency=12)


#Storzenie listy zbiorów o rówżnym poziomie agregacji i wizulizacja efektu

Wszystkie_agr_m <- tsaggregates(ts_dane_m)
plot(Wszystkie_agr_m, main="Suma ofert pracy")



# Agregacja do poziomu tygodniowego z agregacją i wizulizacją tak jak wyżej

ilosc_czas_d <- data_11_19_v2[kod_woj == "30",.N,by = data_przyj_oferty]

ilosc_czas_d <- ilosc_czas_d[order(data_przyj_oferty)]

xts_dni <- as.xts(ilosc_czas_d$N,order.by=as.Date(ilosc_czas_d$data_przyj_oferty)) 
ilosc_czas_w <- apply.weekly(xts_dni,sum)
ilosc_czas_w <- as.data.frame(ilosc_czas_w)


dane_w <- ilosc_czas_w[,1]
names(dane_w) <- "Liczba_ofert"


ts_dane_w <- ts(dane_w, start=c(2011, 1), frequency=52)

Wszystkie_agr_w <- tsaggregates(ts_dane_w)
plot(Wszystkie_agr_w, main="Suma miejsc pracy")


# Analogiczna sytuacja dla zawodów, ale tylko w ujęciu tygodniowym
# Wybór zawodu lub grupy zawodów odbywa się poprzez zmiane kodu w str_detect()

naz_zaw <- ilosc_zaw %>% filter(str_detect(kod_zawodu,"722204")) %>% select(1) %>% distinct()

zaw_czas <- data_11_19_v2[nazwa_zawodu %in% naz_zaw[,1],]

ilosc_czas_d <- zaw_czas[,.N,by = data_przyj_oferty]

ilosc_czas_d <- ilosc_czas_d[order(data_przyj_oferty)]

xts_dni <- as.xts(ilosc_czas_d$N,order.by=as.Date(ilosc_czas_d$data_przyj_oferty)) 
ilosc_czas_w <- apply.weekly(xts_dni,sum)
ilosc_czas_w <- as.data.frame(ilosc_czas_w)


dane_w <- ilosc_czas_w[,1]
names(dane_w) <- "Liczba_ofert"


ts_dane_w <- ts(dane_w, start=c(2011, 1), frequency=52)

Wszystkie_agr_w <- tsaggregates(ts_dane_w)
plot(Wszystkie_agr_w, main=paste("Suma ofert pracy - Przedstaiwciele"))

# Towrzenie prognoz bazowych przy użyciu modelu ARIMA

bazowa <- list()
for(i in 1:5) 
  bazowa[[i]] <- forecast(auto.arima(Wszystkie_agr_w[[i]]))
bazowa[[6]] <- forecast(auto.arima(Wszystkie_agr_w[[6]]), h=2) 

# Tworzenie prognoz uzgodnionych i wizualizacja wyników

uzgodniona <- reconcilethief(bazowa)

main <- paste(names(Wszystkie_agr_w))

for(i in 6:1) {
  ylim <- range(bazowa[[i]]$mean, bazowa[[i]]$x, uzgodniona[[i]]$mean)
  plot(uzgodniona[[i]], main=main[i], fcol='white', ylim=ylim, xlim=c(2011,2022.01))
  lines(uzgodniona[[i]]$mean, col='blue')
}


# Sprawdzanłe prognozy, poprzez zaprognozowanie roku 2019, na podstawie danych do roku 2018.
# Proces analogiczny jak wyżej, mam tylko jeden kod niezależnie czy sprawdzałem błąd dla danych tygodniowych, czy miesięcznych. 
# Jedynie podmieniłem odpowiednie wartości, żeby sprawdzić zgodnośc prognozy dla danych tygodniowych.


zawody_month <- data_11_19_v2[,czas := as.Date(paste(as.character(format(data_11_19_v2$data_przyj_oferty, "%Y-%m")),1, sep = "-"))]

ilosc_czas_m <- zawody_month[czas < "2019-01-01",.N,by = czas]

ilosc_czas_m <- ilosc_czas_m[order(czas)]

dane_m <- ilosc_czas_m[,2]
names(dane_m) <- "Liczba_ofert"


ts_dane_m <- ts(dane_m, start=c(2011, 1), frequency=12)

Wszystkie_agr_m <- tsaggregates(ts_dane_m)

# Dla porównania stworzeni odpowiedniego zbioru z realnymi dnaymi na rok 2019

ilosc_czas_19 <- zawody_month[czas > "2018-12-31",.N,by = czas]

ilosc_czas_19 <- ilosc_czas_19[order(czas)]

dane_19_blad <- ilosc_czas_19[,2]
names(dane_19_blad) <- "Liczba_ofert"


ts_dane_19 <- ts(dane_19_blad, start=c(2019, 1), frequency=12)

Wszystkie_agr_19 <- tsaggregates(ts_dane_19)

# Analogicznie jak wczesniej tworzenie prognoz
bazowa <- list()
for(i in 1:5)
  bazowa[[i]] <- forecast(auto.arima(Wszystkie_agr_m[[i]]))
bazowa[[6]] <- forecast(auto.arima(Wszystkie_agr_m[[6]]), h=2)

uzgodniona <- reconcilethief(bazowa)


blendy_prognoz_arima <- list()
blendy_prognoz_thief <- list()

# Porównanie prognozy z wykorzytsaniem metody ARIMA i temporal hierachy

for(i in 1:6){
  
  prog_arima_2019 <- bazowa[[i]]$mean[1:length(Wszystkie_agr_19[[i]])]
  
  prog_thief_2019 <- uzgodniona[[i]]$mean[1:length(Wszystkie_agr_19[[i]])]
  
  dane_2019 <- Wszystkie_agr_19[[i]][1:length(Wszystkie_agr_19[[i]])]
  
  blendy_prognoz_arima[i] <- sum(abs(dane_2019-prog_arima_2019)/dane_2019*100)/length(dane_2019)
  
  blendy_prognoz_thief[i] <- sum(abs(dane_2019-prog_thief_2019)/dane_2019*100)/length(dane_2019)
  
}

mean(unlist(blendy_prognoz_arima))

mean(unlist(blendy_prognoz_thief))






