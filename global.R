#import library
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse) 
library(plotly) 
library(glue) 
library(scales) 
library(ggpubr)
library(DT)
library(lubridate)

#load dataset
youtube <- read_csv("youtubetrends.csv")

youtube_clean <- youtube %>% 
  mutate(trending_date = ymd(trending_date),
         publish_time = ymd_hms(publish_time),
         year_trending = year(trending_date),
         publish_weekend = ifelse(test = publish_wday == "Saturday" | 
                                    publish_wday == "Sunday", 
                                  yes = "Weekend",
                                  no = "Weekday"),
         channel_title = as.factor(channel_title),
         category_id = as.factor(category_id),
         publish_when = as.factor(publish_when),
         publish_wday = factor(x = publish_wday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                            "Friday", "Saturday", "Sunday")))

theme_lbb3 <- theme(legend.key = element_rect(fill="black"),
                    legend.background = element_rect(color="white", fill="#263238"),
                    plot.subtitle = element_text(size=6, color="white"),
                    panel.background = element_rect(fill="#ead3f5"),
                    panel.border = element_rect(fill=NA),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(color="#e3c7f0", linetype=2),
                    panel.grid.minor.y = element_blank(),
                    plot.background = element_rect(fill="#270d40"),
                    text = element_text(color="white"),
                    axis.text = element_text(color="white")
)