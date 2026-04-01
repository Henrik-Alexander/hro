######
# Project: Presentation for LNdW and CDU-Kreisverband
# Purpose: Visualising the population projections
# Authors: Henrik Schubert & Johann Behrendt
# E-mail: schubert@demogr.mpg.de
# Date: 01/04/2026
#######

#Description
library(readxl)
library(tidyverse)
library(dplyr)


##################visualisation of the past 

# Load the graphic template
source("code/graphic_template.R")

# Load the insgesamt data
Rostock_past <- read_csv("data/insgesamt.csv")

#wide format 
Rostock_past_final <- Rostock_past %>%
  group_by(year)%>%
  summarise(Gesamtanzahl = sum(einwohnerzahl))%>%
  mutate(Jahr = year)%>%
  select(Jahr, Gesamtanzahl)


ggplot(data=Rostock_past_final, mapping = aes(Jahr, Gesamtanzahl))+
  geom_line(linetype="dashed",color= "2c3e50")+
  ylim(190000,220000)+
  theme_minimal()

##########pop forecast 

#Load data ---
data <- read_excel(path = "projection/HRO_Ergebnisse_2023-2040_AG.xlsx")
head(data)

#Analysis / Visualisation ----

#Merge the age groups 
HRO_years <- data %>%
  group_by(Jahr)%>%
  summarise(Gesamtanzahl = sum(Anzahl,na.rm=T))%>%
  filter(Jahr > 2024)


#Plot the overall projection
ggplot(data=HRO_years, mapping = aes(Jahr, Gesamtanzahl))+
  geom_line(linetype="dashed",color= "2c3e50")+
  ylim(190000,220000)+
  theme_minimal()


HRO_past_future <- bind_rows(Rostock_past_final, HRO_years)

ggplot(data=HRO_past_future, mapping = aes(Jahr, Gesamtanzahl)) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "grey") +
  geom_line(data = filter(HRO_past_future, Jahr <= 2024), linetype = "solid", size = 1) +
  geom_point(data = filter(HRO_past_future, Jahr <= 2024), size = 3) +
  geom_line(data = filter(HRO_past_future, Jahr >= 2024), linetype = "dashed", size = 1) +
  scale_x_continuous(n.breaks = 10, expand = c(0, 0.2)) +
  scale_y_continuous("Bevölkerungsgröße", labels = scales::label_number(scale = 1e-3, suffix = " K"), expand = c(0.3, 0), n.breaks = 8) 
ggsave(filename = "Figures/projection_pop_size.pdf", height = 18, width = 22, unit = "cm")

#Additional visualisations ----
#Plot the projection for children vs working adults vs pensioniers 
HRO_work <- data %>%
  mutate(Lebensphase = case_when(
    AG %in% c("unter 3","3 bis unter 6","6 bis unter 10","10 bis unter 15","15 bis unter 18")~ "Kinder und Jugendliche",
    AG %in% c("18 bis unter 25","25 bis unter 45","45 bis unter 65")~ "Erwerbstätige",
    AG %in% c("65 bis unter 80","80+")~"Rentner"))%>%
  group_by(Lebensphase, Jahr)%>%
  summarise(Summe = sum(Anzahl))


# Plot the age groups
ggplot(data=HRO_work, mapping = aes(Jahr, Summe,color=Lebensphase, group=Lebensphase, shape = Lebensphase))+
  geom_line() +
  geom_point() +
  geom_text(data=subset(HRO_work, Jahr == 2030), aes(label = Lebensphase), vjust = -1, fontface = "bold", size = 6) +
  scale_x_continuous(n.breaks = 10, expand = c(0, 0.2)) +
  scale_y_continuous("Bevölkerungsgröße", labels = scales::label_number(scale = 1e-3, suffix = " K"), expand = c(0.2, 0), n.breaks = 8) +
  guides(color = "none", shape = "none") +
  scale_colour_manual(values=c(mpidr_blue, mpidr_red, mpidr_green))

ggsave(filename = "Figures/projection_dependency_counts.pdf", height = 18, width = 22, unit = "cm")

# Estimate the dependency ratios
HRO_ratios <- HRO_work %>% 
  pivot_wider(names_from = "Lebensphase", values_from = "Summe") %>% 
  mutate(kind_ratio = Erwerbstätige / `Kinder und Jugendliche`,
         rentner_ratio = Erwerbstätige / Rentner,
         abhängiger_ratio = Erwerbstätige / (`Kinder und Jugendliche` + Rentner)) %>% 
  pivot_longer(cols = ends_with("ratio"))

# Create the ratio figure
ggplot(data=HRO_ratios, aes(x=Jahr, y=value,color=name, group=name, shape = name))+
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_point() +
  geom_text(data=subset(HRO_ratios, Jahr == 2030), aes(label = paste("pro", str_to_title(str_remove(name, "_ratio")))), vjust = -1, fontface = "bold", size = 6) +
  scale_x_continuous(n.breaks = 10, expand = c(0, 0.2)) +
  scale_y_log10("Erwerbstätige",limits = c(1, 5), n.breaks = 10) +
  guides(color = "none", shape = "none") +
  scale_colour_manual(values=c(mpidr_blue, mpidr_red, mpidr_green))
  
ggsave(filename = "Figures/projection_dependency_ratio.pdf", height = 18, width = 22, unit = "cm")


#Plot the projection for the different age groups using a barplot 
data_diff_2023_2040 <- data %>%
  pivot_wider(names_from = Jahr,values_from = "Anzahl")%>%
  select("AG","2023","2040") %>%
  mutate(Veränderung = `2040` - `2023`)


# Sort the data
data_diff_2023_2040 <- data_diff_2023_2040 %>%
  mutate(AG = factor(AG, levels=c("unter 3", "3 bis unter 6", "6 bis unter 10", 
                                  "10 bis unter 15", "15 bis unter 18", "18 bis unter 25",
                                  "25 bis unter 45","45 bis unter 65","65 bis unter 80",
                                  "80+")))

# Absolute Veränderung
ggplot(data_diff_2023_2040, aes(x=AG,y=Veränderung,fill=Veränderung>0))+
  geom_col(color="white")+
  geom_hline(yintercept = 0) +
  geom_text(aes(label = round(Veränderung)), hjust = -0.2, family = "serif", fontface = "bold", size = 6) +
  # labs(title = "Absolute Veränderungen der Bevölkerungsanzahl von 2023 zu 2040 nach Altersgruppen")+
  scale_x_discrete("Altersgruppe") + 
  scale_y_continuous("Absolute Bevölkerungsveränderung") +
  coord_flip()+
  scale_fill_manual(values = c("TRUE"  = "#2ecc71","FALSE" = "#e74c3c"), guide = "none")
ggsave(filename = "Figures/projection_pop_change_absolute.pdf", height = 18, width = 22, unit = "cm")


ggplot(data_diff_2023_2040, aes(x=AG,y=Veränderung/`2023`,fill=Veränderung>0))+
  geom_col(color="white")+
  geom_hline(yintercept = 0) +
  geom_text(aes(label = paste0(round(Veränderung/`2023` * 100, 1), "%"), y = 1.1 * Veränderung/`2023`),  family = "serif", fontface = "bold", size = 6) +
  # labs(title = "Absolute Veränderungen der Bevölkerungsanzahl von 2023 zu 2040 nach Altersgruppen")+
  scale_x_discrete("Altersgruppe") + 
  scale_y_continuous("Bevölkerungsveränderung (%)", labels = scales::percent, n.breaks = 10) +
  coord_flip()+
  scale_fill_manual(values = c("TRUE"  = "#2ecc71","FALSE" = "#e74c3c"), guide = "none")
ggsave(filename = "Figures/projection_pop_change_relative.pdf", height = 18, width = 22, unit = "cm")
# hjust = ifelse(Veränderung>0, 0.5, -0.5)


## Plot the relative population change

ggplot(data = data_diff_2023_2040, aes(x = AG, y = `2023`)) +
  geom_point(size = 3) +
  geom_segment(aes(x = AG, y = `2023`, yend = `2040`, colour = ifelse(`2040` > `2023`, "Wächst", "Schrumpft")), linewidth = 1.5, arrow = grid::arrow(type = "closed", length = unit(0.1, "inches"), angle = 25)) +
  coord_flip() +
  scale_x_discrete("Altersgruppe") +
  scale_y_continuous("Bevölkerung (2023 vs. 2040)", n.breaks = 10, labels = scales::label_number(scale = 1e-3, suffix = " K")) +
  scale_color_manual("", values = c("red", "forestgreen")) +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.2))
ggsave(filename = "Figures/projection_change_arrows.pdf", height = 15, width = 25, unit = "cm")



### END #######################################################################