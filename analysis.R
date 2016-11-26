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
                      "Purdue", "Rutgers", "Spartans", "Iowa", "Michigan", "Maryland",
                      "Nebraska", "Minnesota", "Wisconsin")

data <- plyr::ldply(data_list)

str(data)

data$date <- lubridate::ymd_hms(data$status_published)
data$year <- lubridate::year(data$date)
data$month <- lubridate::month(data$date)
data$printed_date <- as.numeric(paste0(data$year, ".", data$month))

str(data)

data_ss <- data %>%
    filter(year > 2010) %>% 
    group_by(printed_date, .id) %>%
    summarize(num = sum(num_reactions))

colorCount = 14
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette(colorCount)

length(unique(data_ss$printed_date))

72/12

str(data_ss)

ggplot(data_ss, aes(x = printed_date, y = num, color = .id, group = .id)) +
    geom_line() +
    geom_point() +
    facet_wrap( ~ .id) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = getPalette(colorCount)) +
    scale_x_discrete(breaks = 1:6, labels = c("2011", "2012", "2013", "2014", "2015", "2016")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
    ylab(NULL) +
    xlab(NULL) +
    theme(text =element_text(size = 15, family = "Georgia")) +
    ggtitle("Interactions with B1G Facebook Pages (2011 - 2016)") +
    theme(legend.position = "bottom") +
    labs(color = NULL)

ggsave("b1gpages.png")
