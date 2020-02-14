library(fpp3)
library(sugrrants)

### Set Theme
thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

### Explore Time-series Plots
vic_elec_daily <- vic_elec %>% 
  index_by(Date = date(Time)) %>% 
  summarise(
    Demand = sum(Demand) / 1e3, 
    Temperature = max(Temperature), 
    Holiday = any(Holiday)) %>% 
  mutate(
    Temperature2 = I(pmax(Temperature - 20, 0)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday", 
      TRUE ~ "Weekend"))

fit <- vic_elec_daily %>% tail(100) %>% 
  model(m = ARIMA(log(Demand) ~ Temperature))




vic_next_day <- new_data(vic_elec_daily, 1) %>%
  mutate(Temperature = 26, Day_Type = "Holiday")
forecast(elec_model, vic_next_day)

fit %>% 
  forecast(h = 100) %>% 
  autoplot(vic_elec_daily)

# plot.point <- 1000
# 
# fit <- vic_elec_daily[1:(plot.point-1),] %>%
#   model(fit = ARIMA(log(Demand) ~ Temperature + HDD + Holiday))
# 
# forecast(fit, vic_elec_daily[plot.point:(plot.point+200),]) %>%
#   autoplot(vic_elec_daily[(plot.point-50):(plot.point+200),]) + theme_bw()




  
  
  
  
  
  





