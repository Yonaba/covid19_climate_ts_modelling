setwd("D:/MOD_COVID19/MOD")
Sys.setenv(TZ = "UTC")

library(zoo)
library(COVID19)
library(ggplot2)
library(tidyverse)

if.na <- function(value) return (ifelse(is.na(value),0,value))

s_date <- "2020-01-01"
e_date <- "2022-09-30"
N_DAYS <- as.numeric((as.Date(e_date) - as.Date(s_date)) + 1)

df <- read.csv("data/coord_africa.csv",header = T, sep = ",", dec = ".")
countries <- df$Code_ISO

data <- data.frame(matrix(nrow = N_DAYS, ncol = 0))
for (iso_code in countries) {
  print(iso_code)
  #iso_code <- "BFA"
  covid_cases <- read.csv(paste0("data/covid_daily/",iso_code,"_COVID.csv"),
                 header = TRUE, sep = ",", dec = ".")[,2]
  
  cc <- append(rep(0,N_DAYS-length(covid_cases)), covid_cases)
  data[,iso_code] <- cc
}

dates <- seq(as.Date(s_date), as.Date(e_date), by = "day")

dates <- rep(dates, ncol(data))
cname <- rep(df$Pays, each = N_DAYS)

covid <- data.frame(dates, cname, stack(data))
colnames(covid) <- c("date", "cname", "cases","iso3")

cov_case_curve <- covid %>%
  select(date, cname, iso3, cases) %>%
  drop_na(iso3) %>%
  group_by(iso3) %>%
  arrange(date) %>%
  mutate(cu_cases = cases) %>%
  filter(cu_cases > 99) %>%
  mutate(days_elapsed = (date - as.Date(s_date)),
         end_label = ifelse(date == max(date), cname, NA))

covid_cases <- cov_case_curve %>%
  group_by(cname) %>%
  filter(cu_cases == max(cu_cases)) %>%
  ungroup()

covid_cases <- tail(covid_cases, 54)

covid_cases <- covid_cases %>%
  select(iso3, cname, cu_cases) %>%
  mutate(days_elapsed = 1, 
         cu_cases = max(cov_case_curve$cu_cases))

cov_case_curve_bg <- cov_case_curve %>% 
  select(-cname) %>%
  filter(iso3 %in% covid_cases$iso3) 

cov_case_curve_endpoints <- cov_case_curve %>% 
  filter(iso3 %in% covid_cases$iso3) %>%
  group_by(iso3) %>%
  filter(cu_cases == max(cu_cases)) 

cov_case_curve_endpoints <- tail(cov_case_curve_endpoints, 54)

cov_case_curve_endpoints <- cov_case_curve_endpoints %>%
  select(cname, iso3, days_elapsed, cu_cases) %>%
  ungroup()

cov_case_sm <- cov_case_curve  %>%
  filter(iso3 %in% covid_cases$iso3) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cu_cases)) + 
  # The line traces for every country, in every panel
  geom_line(data = cov_case_curve_bg, 
            aes(group = iso3),
            size = 0.15, color = "gray80") + 
  # The line trace in red, for the country in any given panel
  geom_line(color = "firebrick",
            lineend = "round") + 
  # The point at the end. Bonus trick: some points can have fills!
  geom_point(data = cov_case_curve_endpoints, 
             size = 1.1, 
             shape = 21, 
             color = "firebrick",
             fill = "firebrick2"
  ) + 
  # The country label inside the panel, in lieu of the strip label
  geom_text(data = covid_cases, 
            mapping = aes(label = cname), 
            vjust = "inward", 
            hjust = "inward",
            fontface = "bold", 
            color = "firebrick", 
            size = 3) + 
  # Log transform and friendly labels
  scale_y_log10(labels = scales::label_number_si()) + 
  # Facet by country, order from high to low
  facet_wrap(~ reorder(cname, -cu_cases), ncol = 5) + 
  labs(x = "Days (starting from January 1, 2020)", 
       y = "Log(Cumulative confirmed cases)", 
       title = "COVID-19 cumulative confirmed cases in African countries", 
       subtitle = "Period: January 1, 2020 to September 30, 2022", 
       caption = "Source : JHU/CSSE") + 
  theme(plot.title = element_text(size = rel(1.75), face = "bold"),
        plot.subtitle = element_text(size = rel(1.5)),
        plot.caption = element_text(size = rel(1.25)),
        # turn off the strip label and tighten the panel spacing
        strip.text = element_blank(),
        panel.spacing.x = unit(-0.05, "lines"),
        panel.spacing.y = unit(0.2, "lines"),
        axis.text.y = element_text(size = rel(1)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1)),
        legend.text = element_text(size = rel(1.5)))

#cov_case_sm

ggsave("graph/cov_case_sm.png", cov_case_sm, width = 20, height = 35, dpi = 400, scale = 0.75)
print("finished")