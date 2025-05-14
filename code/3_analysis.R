#####
# Project: Open data for city of Rostock
# Purpose: Load the data from the platform
# Author: Henrik-Alexander Schubert
# Date: 06.05.2025
###

rm(list = ls()); gc(TRUE); options(scipen = 999)


# Load the packages
library(sf)
library(tidyverse)
library(ggrepel)
library(readxl)

# Set the graphic scheme
source("code/graphic_template.R")

# Load the data
files <- list.files("data")
lapply(files, FUN=function(file) {
  print(file)
  tmp <- read.csv(file.path("data", file))
  assign(str_remove(file, ".csv"), tmp, envir = .GlobalEnv)
  rm(tmp)
  })

# Define the district selector
select_districts <- c("Kröpeliner-Tor-Vorstadt",  "Lütten Klein",  "Stadtmitte", "Warnemünde", "Toitenwinkel")

# Load the map
map <- read_sf(list.files("map", pattern = "stadtbereiche.shp", full.names=T))

# Load the base map (created in 2_combine_data.R)
load("data/base_map.Rda")

# Plot the base_map
ggsave(plot=base_map, filename="figures/base_rostock.pdf", height=20, width=15, unit="cm")

# Load the population data from the State Mecklenburg-Vorpommern
pop_proj_hro <- read_xlsx(list.files("raw", pattern="MV.xlsx", full.names=T))
pop_proj_hro <- pop_proj_hro[pop_proj_hro$Region=="Rostock", ]
pop_proj_hro <- pivot_longer(pop_proj_hro, cols=!Region, names_to="year", values_to="pop")
pop_proj_hro$year <- as.numeric(pop_proj_hro$year)
pop_proj_hro <- pop_proj_hro[!is.na(pop_proj_hro$pop), ]

# Functions -----------------------------------

plot_indicator_map <- function(indicator_data, indicator, map_data=map, year=2023) {
  
  # Merge the data
  map_indicator <- left_join(x=map_data,
                             y=indicator_data,
                             by=c("bez"="stadtbereich_bezeichnung", "code"="stadtbereich_code"))
  
  
  # Create the base map
  p <- base_map +
    geom_sf(data=map_indicator, aes(fill={{indicator}}), linewidth=1.1, colour="black") +
    geom_sf_text(data=map_labels, aes(label=gsub("-| |/", "\n", bez)), colour="white")
  
  return(p)
  
}


# 1. Analysis -------------------------------






## 1.2 Population growth -----------------------

# Plot the projection
ggplot(data=pop_proj_hro, aes(x=year, y=pop, colour="Projektion")) +
  geom_point(size=3) +
  geom_line() +
  #geom_smooth(linewidht=2, alpha= 0.3, method="lm", formula = "y~x") +
  scale_y_continuous("Einwohnerzahl", expand=c(0, 0), n.breaks=10, limits = c(196000, 215000)) +
  scale_x_continuous("Jahr", expand=c(0, 0.1), n.breaks=11, limits=c(2002, 2023)) +
  scale_colour_manual("Daten:", values=c(mpidr_grey, mpidr_green))


# Plot the population developmetn in the city
rostock_growth <- insgesamt %>% 
  group_by(year) %>% 
  summarise(Rostock = sum(einwohnerzahl), .groups="drop"
  ) 

ggplot(data=pop_proj_hro) +
  geom_point(aes(x=year, y=pop, colour="Projektion", shape="Projektion"), size=4) +
  #geom_line(aes(x=year, y=pop, colour="Projektion"), linewidth=1.5) +
  geom_line(data=rostock_growth, aes(x=year, y=Rostock, colour="Beobachtet"), linewidth=1.5) + 
  geom_point(data=rostock_growth, aes(x=year, y=Rostock, colour="Beobachtet", shape="Beobachtet"), size=3) +
  geom_label_repel(data=subset(rostock_growth, year%in%c(2005, 2023)), aes(x=year, y=Rostock, label=paste0(round(Rostock/1000),"k")), colour=mpidr_red, nudge_y = 3000, size=8) + 
  #geom_smooth(linewidht=2, alpha= 0.3, method="lm", formula = "y~x") +
  scale_y_continuous("Einwohnerzahl", expand=c(0, 0), n.breaks=10, limits = c(196000, 215000)) +
  scale_x_continuous("Jahr", expand=c(0, 0.1), n.breaks=11, limits=c(2002, 2023)) +
  scale_colour_manual("Daten:", values=c(mpidr_red, mpidr_green)) +
  scale_shape_manual("Daten:", values = c(17, 15)) +
  theme(legend.position = c(0.2, 0.8),
        legend.background = element_rect(colour="grey"))
ggsave(filename="figures/pop_proj_rostock.pdf", height=15, width=22, unit="cm")



# Population size
ggplot(data=insgesamt, aes(x=year, y=einwohnerzahl, group=stadtbereich_bezeichnung)) +
  geom_line(colour="grey") + 
  geom_point(data=subset(insgesamt, stadtbereich_bezeichnung %in% select_districts), aes(colour=stadtbereich_bezeichnung), size=3) +
  geom_line(data=subset(insgesamt, stadtbereich_bezeichnung %in% select_districts), aes(colour=stadtbereich_bezeichnung), linewidth=1.3) +
  geom_text(data=subset(insgesamt, year==max(insgesamt$year) & stadtbereich_bezeichnung %in% select_districts), aes(label=stadtbereich_bezeichnung, colour=stadtbereich_bezeichnung), vjust="bottom", nudge_y = 50) +
  scale_y_continuous("Einwohnerzahl", expand=c(0.03, 0), n.breaks=10) +
  scale_x_continuous("Jahr", expand=c(0, 0), limits = c(min(insgesamt$year), max(insgesamt$year)+1)) + 
  guides(colour="none")
ggsave(filename="figures/pop_districts.pdf",
       height=15, width=25, unit="cm")


# Has Rostock grown or shrunken
population_growth <- bewegung_gesamt %>% 
  group_by(stadtbereich_bezeichnung, stadtbereich_code) %>% 
  summarise(abs_bestandsveraenderung =sum(abs_bestandsveraenderung ),
            duration=max(year)-min(year),
            start=min(year),
            end=max(year), .groups="drop")
plot_population_growth <- plot_indicator_map(population_growth, abs_bestandsveraenderung)
plot_population_growth +
  ggtitle("Bevölkerungswachstum (2005-2019)") +
  scale_fill_gradient2(guide = guide_legend(ncol=1), low=mpidr_red, mid=mpidr_grey, high=mpidr_blue, n.breaks=8) +
  theme(legend.title = element_blank())
ggsave("figures/pop_growth_map.pdf", height=20, width=15, unit="cm")


ggplot(data=population_growth, aes(x= fct_reorder(stadtbereich_bezeichnung, abs_bestandsveraenderung), y=abs_bestandsveraenderung)) +
  geom_col(aes(fill=abs_bestandsveraenderung)) +
  geom_hline(yintercept = 0, linewidth=2) +
  coord_flip() +
  scale_fill_gradient2(guide = guide_legend(ncol=1), low=mpidr_red, mid=mpidr_grey, high=mpidr_blue, n.breaks=8) +
  scale_y_continuous("Bevölkerungswachstum", n.breaks=10) +
  guides(fill="none") +
  theme(
    axis.title.y=element_blank()
  ) +
  ggtitle("Bevölkerungswachstum (2005-2019)")

ggsave(filename="figures/popoulation_growth_barchart.pdf",
       height=20, width =20, unit="cm")


# Make line graph
maximum_bevoelkerung <- 10
rostock_growth <- bewegung_gesamt %>% 
  group_by(year) %>% 
  summarise(Rostock = sum(abs_bestandsveraenderung))
ggplot(data= subset(bewegung_gesamt, stadtbereich_bezeichnung %in% select_districts), aes(year, abs_bestandsveraenderung)) +
  geom_hline(yintercept = 0) +
  #geom_col(data= subset(rostock_growth, stadtbereich_bezeichnung %in% select_districts), aes(x=year, y=Rostock/maximum_bevoelkerung), fill="white", col="black") +
  geom_col(aes(fill=stadtbereich_bezeichnung)) +
  geom_point(aes(colour=stadtbereich_bezeichnung)) +
 # geom_text(data=subset(bewegung_gesamt, year == max(year)), aes(label = stadtbereich_bezeichnung, colour=stadtbereich_bezeichnung), nudge_x=1) +
  scale_x_continuous(expand = c(0, 0), limits = c(2004.5, 2023.5)) +
  scale_y_continuous("Bevölkerungsveränderung") +
  scale_fill_manual(values = c(mpidr_green, mpidr_blue, mpidr_orange, mpidr_grey, mpidr_red)) +
  scale_colour_manual(values = c(mpidr_green, mpidr_blue, mpidr_orange, mpidr_grey, mpidr_red)) +
  
  guides(colour = "none", shape="none", fill="none") +
  facet_wrap(~ stadtbereich_bezeichnung)
ggsave(filename="figures/pop_change_district.pdf", height=18, width=25, unit = "cm")

# Make the plot
plot <- plot_indicator_map(haushaltsstruktur, indicator=anteil_alleinerziehende_an_mit_kindern, year=2023)
plot +
  scale_fill_viridis_c("", guide = guide_legend(ncol=1), n.breaks=10) +
  theme(legend.key.width = unit(0.5, "cm")) +
  ggtitle("Anteil Alleinerziehende an Haushalten mit Kindern")

# Natural population change ------------------------------------------------------

# Plot the natural population change
plot <- plot_indicator_map(bewegung_natuerlich, indicator=geburten_sterbesaldo, year=2022)
plot +
  scale_fill_gradient2("Geburten-Sterbesaldo", guide = guide_legend(ncol=1), low=mpidr_red, mid=mpidr_grey, high=mpidr_blue, n.breaks=8) +
  theme(legend.key.width = unit(0.5, "cm")) +
  ggtitle("Naturürliche Bevölkerungsveränderung (Geburten-Sterbefälle)")

# Plot the natural population change
plot <- plot_indicator_map(bewegung_natuerlich, indicator=geburten_sterbesaldo_je_1000, year=2022)
plot +
  scale_fill_gradient2("Geburten-Sterbesaldo \n(pro 100 Einwohner)", guide = guide_legend(ncol=1),  low=mpidr_red, mid=mpidr_grey, high=mpidr_blue, n.breaks=8) +
  theme(legend.key.width = unit(0.5, "cm"))

## Sterbefaelle ----------------------------------------------

bewegung_natuerlich %>% 
  group_by(year) %>% 
  summarise(deaths=sum(anzahl_gestorbene), .groups="drop") %>% 
  ggplot(aes(x=year, y=deaths)) +
  geom_line() +
  geom_point() +
  scale_x_continuous("Jahr", expand=c(0, 0), n.breaks=20) +
  scale_y_continuous("Sterbefälle", expand=c(0, 0), n.breaks=10)


bewegung_natuerlich %>% 
  group_by(year) %>% 
  summarise(births=sum(anzahl_lebendgeborene), .groups="drop") %>% 
  ggplot(aes(x=year, y=births)) +
  geom_line() +
  geom_point() +
  scale_x_continuous("Jahr", expand=c(0, 0), n.breaks=20) +
  scale_y_continuous("Geburten", expand=c(0, 0), n.breaks=10)

## Age Structure ---------------------------------------------

# Plot the average age
plot_movement_trend <- plot_indicator_map(alter, durchschnittsalter, year=2023)
plot_movement_trend +
  scale_fill_gradient("Alter", guide = guide_legend(ncol=1),  low=mpidr_blue, high=mpidr_red, n.breaks=8) +
  theme(legend.key.width = unit(1, "cm")) +
  ggtitle("Durchschnittsalter (2023)")
ggsave(filename="figures/durchschnittsalter_district.pdf", height=20, width=15, unit="cm")

# Reshape the data from wide (anzahl und anteil) zu long format
alter <- pivot_longer(alter, cols = starts_with("an"), names_to = c("variable", "age_group"), values_to = "pop", names_pattern = "([a-z]+)(.+)") %>% 
  mutate(age_group = str_remove(age_group, "^_"))

# Clean age group
clean_agegroup <- function(x) {
  agegroup <- x
  agegroup <- str_replace(agegroup, "juenger_", "<")
  agegroup <- str_replace(agegroup, "_aelter", "+")
  agegroup <- str_replace(agegroup, "ae", "ä")
  agegroup <- str_replace(agegroup, "ue", "ü")
  agegroup <- str_replace(agegroup, "_", "-")
  agegroup <- factor(agegroup, levels = c("<3", "3-6", "6-15", "15-25", "25-35", "35-45", "45-55", "55-65", "65-75", "75+"))
  return(agegroup)
}


# Create the mid-age
create_midage <- function(x) {
  age <- numeric(length=length(x))
  age <- (as.numeric(str_extract(x, "^[0-9]+")) + as.numeric(str_extract(x, "[0-9]+$"))) / 2
  age[str_detect(x, "juenger")] <- 1.5
  age[str_detect(x, "aelter")] <- 85
  return(age)
}
  
# Clean the age groups
alter$midage <- create_midage(alter$age_group)
alter$age_group <- clean_agegroup(alter$age_group)
         
# Plot the distribution
ggplot(subset(alter, variable == "anzahl" & year %in% c(2005, 2023) & stadtbereich_bezeichnung %in% select_districts), aes(x=midage, y = pop, group = year, colour=as.factor(year))) + 
  geom_line(linewidth=1.5) + 
  geom_point(size=3) +
  facet_wrap(~ stadtbereich_bezeichnung) +
  scale_colour_manual("Year", values=c(mpidr_blue, mpidr_orange)) +
  scale_x_continuous("Alter", expand = c(0, 1), n.breaks = 10) +
  scale_y_continuous("Bevölkerungsgröße", n.breaks = 6) +
  theme(legend.position=c(0.8, 0.1))

ggsave(filename="figures/bevölkerungsstruktur_rostock_districts.pdf", height=15, width=25, unit="cm")


# Plot the distribution
ggplot(subset(alter, variable == "anteil" & year %in% c(2005, 2023) & stadtbereich_bezeichnung %in% select_districts), aes(x=midage, y = pop, group = year, colour=as.factor(year))) + 
  geom_line(linewidth=1.5) + 
  geom_point(size=3) +
  facet_wrap(~ stadtbereich_bezeichnung) +
  scale_colour_manual("Year", values=c(mpidr_blue, mpidr_orange)) +
  scale_x_continuous("Alter", expand = c(0, 1), n.breaks = 10) +
  scale_y_continuous("Bevölkerungsanteil", n.breaks = 6) +
  theme(legend.position=c(0.8, 0.1))

ggsave(filename="figures/altersstruktur_rostock_districts.pdf", height=15, width=25, unit="cm")

# Estimate the population count in each age group
alter_rostock <- alter %>% 
  filter(variable == "anzahl") %>% 
  group_by(year, midage, age_group) %>% 
  summarise(pop = sum(pop), .groups="drop")


# Plot the age structure in Rostock
ggplot(subset(alter_rostock, year %in% c(2005, 2023)), aes(x=age_group, y = pop, group = year, fill=as.factor(year))) + 
  geom_col(linewidth=1.5, position = position_dodge()) + 
  #scale_fill_viridis_d("Year:") +
  scale_fill_manual("Jahr:", values=c(mpidr_blue, mpidr_orange)) +
  scale_x_discrete("Altersgruppe") +
  ggtitle("Altersstruktur in Rostock") + 
  scale_y_continuous("Bevölkerung", limits = c(0, 33000), n.breaks = 10, expand=c(0, 0)) +
  theme(
    legend.position=c(0.1, 0.8)
  )

ggsave(filename="figures/altersstruktur_rostock.pdf", height=15, width=25, unit="cm")

## Sex balance -----------------------------------------

# Estimate the sex ratio
geschlecht$sr <- with(geschlecht, anzahl_maennlich/anzahl_weiblich)

# Plot the average age
plot_sr_map  <- plot_indicator_map(geschlecht, sr, year=2023)
plot_sr_map +
  scale_fill_gradient2("", guide = guide_legend(ncol=1), low=mpidr_red, mid=mpidr_grey, midpoint=1, high=mpidr_blue, n.breaks=10) +
  theme(legend.key.width = unit(1, "cm")) +
  ggtitle("Geschlechterverhältnis (2023)")
ggsave(filename="figures/sr_district_map.pdf", height=20, width=15, unit="cm")


# Plot the sex ratio
geschlecht %>% 
  group_by(year) %>% 
  summarise(sr = sum(anzahl_maennlich)/sum(anzahl_weiblich), .groups="drop") %>% 
  ggplot(aes(x=year, y = sr)) + 
    geom_hline(yintercept = 1) +
    geom_line(linewidth=1.3) +
    geom_point(size=3) +
    scale_x_continuous(expand = c(0, 0.2), n.breaks=10) +
    scale_y_continuous("Geschlechterverältnis")
ggsave(filename="figures/sr_rostock.pdf", height=15, width=20, unit="cm")


geschlecht <- pivot_longer(geschlecht, cols = starts_with("an"), names_to = c("variable", "sex_group"), values_to = "pop", names_pattern = "([a-z]+)(.+)") %>% 
  mutate(sex_group = str_remove(sex_group, "^_"))


ggplot(data=subset(geschlecht, variable=="anzahl" & stadtbereich_bezeichnung %in% select_districts), aes(x=year, y = pop, colour=sex_group)) + 
  geom_line(linewidth=1.5) + 
  geom_point(size=3) +
  scale_colour_manual("Geschlecht:", values = c(mpidr_blue, mpidr_red)) +
  scale_x_continuous("Jahr", expand = c(0, 0.5)) +
  scale_y_continuous("Bevölkerung") +
  facet_wrap(~stadtbereich_bezeichnung, scales = "free_y") +
  theme(legend.position = c(0.8, 0.2))
ggsave(filename="figures/sr_rostock_districts.pdf", height=15, width=20, unit="cm")


geschlecht_rostock <- geschlecht %>% 
  group_by(year, variable, sex_group) %>% 
  summarise(pop=sum(pop), .groups="drop") %>% 
  filter(variable == "anzahl") 
ggplot(data=geschlecht_rostock, aes(x=year, y=pop, group=sex_group, colour=sex_group)) +
  geom_text(data=subset(geschlecht_rostock, year==2023), aes(label=sex_group), nudge_y=300, nudge_x=-0.5) +
  geom_line(linewidth=2) +
  geom_point(size=4) +
  scale_colour_manual(values = c(mpidr_blue, mpidr_red)) +
  scale_y_continuous("Bevölkerung", expand=c(0.05, 0), n.breaks=10) +
  scale_x_continuous("Jahr", n.breaks=10, expand=c(0, 0.4)) +
  guides(colour="none")


## Migration ------------------------------------------------


# Which districts have grown are shunken
movement_trend <- bewegung_raeumlich %>% 
  group_by(stadtbereich_bezeichnung, stadtbereich_code) %>% 
  summarise(wanderungssaldo=sum(wanderungssaldo),
            duration=max(year)-min(year),
            start=min(year),
            end=max(year), .groups="drop")
plot_movement_trend <- plot_indicator_map(movement_trend, wanderungssaldo)
plot_movement_trend +
  scale_fill_gradient2("Wanderungssaldo (2005-2019)", guide = guide_legend(ncol=1), low=mpidr_red, mid=mpidr_grey, high=mpidr_blue, n.breaks=8)
ggsave(filename="figures/wanderungssaldo_map.pdf")

ggplot(data=movement_trend, aes(x= fct_reorder(stadtbereich_bezeichnung, wanderungssaldo), y=wanderungssaldo)) +
  geom_col(aes(fill=wanderungssaldo)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_fill_gradient2(low=mpidr_red, mid=mpidr_grey, high=mpidr_blue) +
  guides(fill="none") +
  theme(
    axis.title.y=element_blank()
  ) +
  ggtitle("Wanderungssaldo (2005-2019)")

ggsave(filename="figures/wanderungssaldo_districts.pdf",
       height=12, width =10, unit="cm")



# Migration over time in the dstricts
mig_long <- bewegung_raeumlich %>% 
  pivot_longer(cols = c("anzahl_zuzuege", "anzahl_wegzuege"), names_to="direction", values_to="migration")
ggplot(data=subset(mig_long, stadtbereich_bezeichnung%in%select_districts), aes(x=year)) +
  geom_point(aes(y=migration, colour=direction, group=direction), size=3) +
  geom_line(aes(y=migration, colour=direction, group=direction), linewidth=1.5) +
  #geom_ribbon(data=bewegung_raeumlich, aes(ymin=anzahl_zuzuege, ymax=anzahl_wegzuege, fill=ifelse(anzahl_wegzuege>anzahl_zuzuege, "schwund", "wachstum")), alpha=.5) +
  scale_colour_manual("Richtung", values=c(mpidr_blue, mpidr_orange), labels = c("Wegzüge", "Zuzüge")) +
  facet_wrap(~ stadtbereich_bezeichnung) +
  scale_x_continuous("Jahr", n.breaks=10) +
  scale_y_continuous("Anazahl an Zu und Wegzuegen", n.breaks=10) +
  theme(
    legend.position = c(0.8, 0.2)
  )


## Decomposing population change ----------------------------------------------


# Merge the natural and the migration data
pop_change <- merge(bewegung_natuerlich, bewegung_raeumlich)
pop_change <- merge(pop_change, bewegung_gesamt)

# Aggregate
pop_change_wide <- pop_change %>% 
  na.omit() %>% 
  group_by(stadtbereich_bezeichnung) %>% 
  summarise(Geburten = sum(anzahl_lebendgeborene),
            Sterbefälle = -sum(anzahl_gestorbene),
            Zuzüge = sum(anzahl_zuzuege),
            Wegzüge = -sum(anzahl_wegzuege),
            nat_pop = sum(geburten_sterbesaldo),
            mig_pop = sum(wanderungssaldo), .groups="drop"
            ) %>% 
  mutate(pop_total = nat_pop + mig_pop)

pop_change_long <- pop_change_wide %>% 
  pivot_longer(cols=!stadtbereich_bezeichnung) %>% 
  group_by(stadtbereich_bezeichnung) %>% 
  mutate(max_val = max(value))
  

  
pop_change_long %>% 
  filter(str_detect(name, "pop")) %>% 
  ggplot(data=., aes(x=fct_reorder(stadtbereich_bezeichnung, max_val), y=value, fill=name)) +
  geom_col(position = position_dodge()) +
  geom_hline(yintercept = 0) +
  scale_fill_manual("Bevölkerungsveränderung", values=c(mpidr_grey, mpidr_blue, mpidr_red), labels = c("Räumlich", "Natürlich", "Insgesamt")) +
  theme(legend.position=c(0.8, 0.2)) + 
  scale_y_continuous("Bevölkerungsveränderung", labels = abs, n.breaks=10) +
  ggtitle("Bevölkerungsveränderung") +
  coord_flip() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.key.width = unit(1, "cm"),
    plot.title.position = "plot",
    legend.background = element_rect(colour="grey"))
ggsave(filename="figures/pop_change_districts.pdf", height=20, width=20, unit="cm")


pop_change_long %>% 
  filter(str_detect(name, "pop") & stadtbereich_bezeichnung %in% select_districts) %>% 
  ggplot(aes(x=name, y=value, fill=name)) +
  geom_col(position = position_dodge()) +
  geom_hline(yintercept = 0) +
  scale_fill_manual("Bevölkerungsveränderung", values=c(mpidr_grey, mpidr_blue, mpidr_red), labels = c("Räumlich", "Natürlich", "Insgesamt")) +
  theme(legend.position=c(0.8, 0.2)) + 
  scale_x_discrete(labels =c("Räumlich", "Natürlich", "Insgesamt")) +
  scale_y_continuous("Bevölkerungsveränderung", labels = abs, n.breaks=10) +
  facet_wrap(~ stadtbereich_bezeichnung) +
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key.width = unit(1, "cm"),
    plot.title.position = "plot",
    legend.background = element_rect(colour="grey"))
ggsave(filename="figures/pop_change_districts_zoom.pdf", height=20, width=25, unit="cm")

  
ggplot(subset(pop_change_long, !str_detect(name, "pop")), aes(x=fct_reorder(stadtbereich_bezeichnung, max_val), y=value, group=name)) +
  geom_hline(yintercept = 0) +
  geom_col(aes(fill=name, group=name), position = position_dodge(), alpha=0.8) +
  coord_flip() +
  scale_fill_manual("Ereignisse", values=c(mpidr_grey, mpidr_blue, mpidr_red, mpidr_green)) +
  scale_y_continuous("Ereignisse", labels = abs, n.breaks=10) +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = c(0.8, 0.2),
    legend.background = element_rect(colour="grey")
  )

ggsave(filename="figures/events_districts.pdf", height=20, width=20, unit="cm")


ggplot(subset(pop_change_long, !str_detect(name, "pop") & stadtbereich_bezeichnung %in% select_districts), aes(x=name, y=value, group=name)) +
  geom_hline(yintercept = 0) +
  geom_col(aes(fill=name, group=name), position = position_dodge(), alpha=0.8) +
  scale_fill_manual("Ereignisse", values=c(mpidr_grey, mpidr_blue, mpidr_red, mpidr_green)) +
  scale_y_continuous("Anzahl der Ereignisse", labels = abs, n.breaks=10) +
  facet_wrap(~ stadtbereich_bezeichnung) +
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.2),
    legend.background = element_rect(colour="grey")
  )


ggsave(filename="figures/events_districts_zoom.pdf", height=20, width=25, unit="cm")


### END ######################################################