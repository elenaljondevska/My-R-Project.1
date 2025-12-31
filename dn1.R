df <- ncrn_birds_forest_R1_BirdDataStandard_as_1_
rm(ncrn_birds_forest_R1_BirdDataStandard_as_1_)
library(dplyr)
gwmp <- df %>%
  filter(UnitCode == "GWMP") %>%
  select(ObservationID, EventYear, VisitNumber, AirTemp) %>% na.omit()
# opis spremenjivke

# leto meritve

library(tidyr)
freq_year <- gwmp |>
  count(EventYear) |>
  complete(EventYear=full_seq(EventYear,1),fill=list(n=0)) |>
  mutate(rel_freq = (n / sum(n))*100,
         cum_n= cumsum(n),
         cum_rel_freq = (cum_n / sum(n)) * 100)
freq_year
#EventYear <- min(freq_year$EventYear):max(freq_year$EventYear)
#cum_rel_freq <- (cum_n / sum(n)) * 100
# ogiva
library(ggplot2)
library(patchwork)
p1<-ggplot(freq_year, aes(x = EventYear, y = cum_n)) +
  geom_line() +
  geom_point() +
  labs(y = "kumulativna frekvenca", x = "leto meritve") +
  scale_x_continuous(breaks = freq_year$EventYear) +
  scale_y_continuous(breaks = freq_year$cum_n)
p2<-ggplot(freq_year, aes(x = EventYear, y = cum_rel_freq)) +
  geom_line() +
  geom_point() +
  labs(y = "kumulativna relativna frekvenca(%)", x = "leto meritve") +
  scale_x_continuous(breaks = freq_year$EventYear) +
  scale_y_continuous(breaks = freq_year$cum_rel_freq)
range(gwmp$EventYear)
p1+p2 


# temp(c°)
  gwmp$temp_group <- cut(
  as.numeric(gwmp$AirTemp),
  breaks = seq(-10, 40, by = 2),
  right = FALSE
)
freq_temp <- gwmp %>%
  count(temp_group) %>%
  mutate(rel_freq =( n / sum(n))*100)

freq_temp
gwmp$AirTemp <- as.numeric(as.character(gwmp$AirTemp))
gwmp <- gwmp %>% filter(!is.na(AirTemp))
ggplot(gwmp, aes(x = AirTemp)) +
  geom_histogram(binwidth = 2,boundary=8,fill="grey30",color="white") +
  stat_bin(binwidth=2, boundary=8, geom="text",
           aes(label=after_stat(count)),vjust=-0.5, size=3)+
  scale_x_continuous(breaks=seq(8,34,by=2))+
  labs(
    x = "Temperatura zraka (°C)",
    y = "Frekvenca",
    title = "Histogram temperature zraka"
  ) + theme_minimal()
mean(gwmp$AirTemp) #20.8389
median(gwmp$AirTemp) #21
sd(gwmp$AirTemp) #3.56505

# RAZLIKA V POVPR.TEMP. MED PRVIM IN DRUGIM OBISKOM LOKACIJE
visit1<-gwmp %>% 
  filter(VisitNumber==1)
visit2<-gwmp %>%
  filter(VisitNumber==2)
mean(visit1$AirTemp)
mean(visit2$AirTemp)
sd(visit1$AirTemp)
sd(visit2$AirTemp)
# boxplot
ggplot(gwmp %>% filter(VisitNumber %in% c(1,2)),
       aes(x = factor(VisitNumber), y = AirTemp)) +
  geom_boxplot() +
  labs(x = "Obisk", y = "Temperatura (°C)")
# t-test
#H0: Povprečni temperaturi ob prvem in drugem obisku sta enaki
#H1: Povprečni temperaturi se razlikujeta

t.test(visit1$AirTemp, visit2$AirTemp, paired = F)


#p-vrednost 2.2e-16 < 0.05 zavrnemo H0
#Zaključek: Povprečni temperaturi ob prvem in drugem obisku se razlikujeta.

# POVEZAVA MED LETOM IN POVPREČNO TEMPERATURO

ggplot(gwmp, aes(x = EventYear, y = AirTemp)) +
  geom_point() +
  geom_smooth(method = "loess")
cor.test(gwmp$EventYear, gwmp$AirTemp, method = "spearman")
# S Spearmanovim koeficientom korelacije smo preverjali povezanost med letom meritve in temperaturo zraka. 
#Izračunani koeficient znaša ρ = 0,34, kar kaže na zmerno pozitivno povezanost med spremenljivkama.
#P-vrednost je manjša od 0,05, zato ničelno hipotezo zavrnemo in sklepamo, da temperatura zraka z leti statistično značilno narašča.

