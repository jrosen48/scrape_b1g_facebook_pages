# Interactions with Presidential candidates' Facebook pages

setwd("~/Dropbox/scrape_b1g_facebook_pages")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

data_list <- list()

list.files("data")

for (i in 1:length(list.files("data"))){
    data_list[[i]] <- readr::read_csv(paste0("data/", list.files("data")[i]))
}

list.files("data")

names(data_list) <- c("Illinois", "Indiana", "Northwestern", "Ohio State", "Penn State",
                      "Purdue", "Rutgers", "Michigan State", "Iowa", "Michigan", "Maryland",
                      "Nebraska", "Minnesota", "Wisconsin")

data <- plyr::ldply(data_list)

str(data)

data$date <- lubridate::ymd_hms(data$status_published)
data$year <- lubridate::year(data$date)
data$month <- lubridate::month(data$date)
data$month
data$printed_date <- as.numeric(paste0(data$year, ".", data$month))

data$printed_date

str(data)

data_ss <-
    data %>%
        filter(year > 2010) %>% 
        group_by(date, .id) %>%
        summarize(num = sum(num_reactions))

str(data_ss)

colorCount = 14
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette(colorCount)
the_palette <- getPalette(colorCount)

# psu #002469
# illinois: #e87722
# indiana: #9d2235
# iowa: #ffcd00
# msu: #173f35
# minn: #FFBF3F
# neb: #c8102e
# nw: #5f259f
# osu: #a2aaad
# pursue: #85714d
# wisc: #ba0c2f
# mich: #00274c
# rut: #000000
# maryland: #ed1c24

the_palette <- c("#e87722", "#9d2235", "#ffcd00", "#ed1c24", 
                 "#00274c", "#FFBF3F", "#c8102e", "#5f259f",
                 "#a2aaad", "#002469", "#85714d", "#000000",
                 "#173f35", "#ba0c2f")

the_palette <- tolower(the_palette)
sort(unique(data_ss$printed_date))

72/12

str(data_ss)

?scale_x_discrete

ggplot(data_ss, aes(x = date, y = num, color = .id, group = .id)) +
    geom_line() +
    facet_wrap( ~ .id) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = the_palette) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
    ylab(NULL) +
    xlab(NULL) +
    theme(text =element_text(size = 15, family = "Georgia")) +
    ggtitle("Reactions to B1G Facebook Page Posts (2011 - 2016)") +
    theme(legend.position = "none") +
    labs(caption = "Data collected on 11-26-2016. More information is at https://github.com/jrosen48/scrape_b1g_facebook_pages.")

ggsave("b1gpages.png")
