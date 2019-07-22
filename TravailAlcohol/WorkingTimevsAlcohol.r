
library(tidyverse)
library(ggthemes)

alcohol <- read.csv('alcohol.csv', skip=4) %>% select(c(1,2, "X2016")) %>% rename(Alcohol = X2016)

head(alcohol)

workhours <- read.csv('workhours.csv') %>% 
    filter(Employment.status == "Total employment" & Time == 2016) %>% 
    select(COUNTRY, Country, Value) %>%
    rename(WorkHours = Value)

head(workhours)

workAlcohol <- merge(alcohol, workhours, by.x='Country.Code', by.y='COUNTRY') %>% 
    select(Country, Country.Code, Alcohol, WorkHours)

head(workAlcohol)

meanAlc <- workAlcohol$Alcohol %>% mean()
meanWork <- workAlcohol$WorkHours %>% mean()

ggplot(data = workAlcohol %>% filter(!Country %in% c('France', 'Japan', 'Korea', 'Mexico', 'Costa Rica', 'Israel')), mapping = aes(WorkHours, Alcohol)) +
    geom_point() +
    geom_point(data = workAlcohol %>% filter(Country == 'Japan'), colour='red') +
    geom_text(data = workAlcohol %>% filter(Country == 'Japan'), colour='red', aes(label=Country), hjust=1, vjust=0.25, nudge_x=-50) +
    geom_point(data = workAlcohol %>% filter(Country == 'France'), colour='blue') +
    geom_text(data = workAlcohol %>% filter(Country == 'France'), colour='blue', aes(label=Country), hjust=1, vjust=0.25, nudge_x=-50) +
    geom_point(data = workAlcohol %>% filter(Country == 'Korea')) +
    geom_text(data = workAlcohol %>% filter(Country == 'Korea'), aes(label=Country), hjust=0, vjust=0.25, nudge_x=50) +
    geom_point(data = workAlcohol %>% filter(Country == 'Mexico')) +
    geom_text(data = workAlcohol %>% filter(Country == 'Mexico'), aes(label=Country), hjust=1, vjust=0.25, nudge_x=-50, nudge_y=-0.25) +
    geom_point(data = workAlcohol %>% filter(Country == 'Costa Rica')) +
    geom_text(data = workAlcohol %>% filter(Country == 'Costa Rica'), aes(label=Country), hjust=1, vjust=0.25, nudge_x=-50) +
    geom_point(data = workAlcohol %>% filter(Country == 'Israel')) +
    geom_text(data = workAlcohol %>% filter(Country == 'Israel'), aes(label=Country), hjust=1, vjust=0.25, nudge_x=-50) +

    geom_text(data = workAlcohol %>% filter(Country == 'Lithuania'), aes(label=Country), hjust=0.5, vjust=0.25, nudge_y=0.35) +

    # adding China which was not in the OECD
    geom_point(data=data.frame(c(1)), aes(47*48, 6.7)) +
    geom_text(data= data.frame(c(1)), aes(47*48, 6.7, label='China'), hjust=1, vjust=0.25, nudge_x=-50, nudge_y=0.25) +

    geom_point(aes(meanWork, meanAlc), shape='+', size = 3, color='gray')+
#     geom_text(aes(500, 5, label='Average'))+
#     geom_text(aes(label=Country)) +
    coord_cartesian(
        expand = F,
        xlim = c(0, 2500),
        ylim = c(0, 20)
    ) +
    labs(
        title = "Alcohol consumption vs working hours in OECD countries (2016)",
        subtitle = "Alcohol consumption is evaluated by liters of pure alcohol per capita",
        caption = "Source: OECD & World DataBank\nBy @JaponMeneATout",
        y = "Alcohol consumption",
        x = "Annual working hours"
    ) +
    theme_economist() +
    theme(
        plot.caption = element_text(hjust=0)
    )

ggsave("alcohol_vs_work.png")
