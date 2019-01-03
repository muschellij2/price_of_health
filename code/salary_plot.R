library(readr)
library(lubridate)
library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)
library(here)

var_info = read_excel(here("salary", "2016.xlsx"), sheet = "Format")
var_info = var_info %>% 
  rename_all(tolower) %>%
  filter(!is.na(varnum))

res = read_rds(here("salary", "salary_data.rds"))
df = res %>% 
  filter(job_title != "TOTAL") %>% 
  filter(hours_work > 0) %>% 
  filter(!is.na(hospital_name)) %>% 
  select(year, hospital_name, job_title,
         hours_work, num_employees,
         total_salary,
         dollar_per_hour, total_cost) %>% 
  mutate(avg_salary_per_pay = total_salary / num_employees) %>% 
  arrange(desc(dollar_per_hour), desc(num_employees))

nurse = df %>% 
  filter(grepl("NURSE", toupper(job_title))) 

nurse5 = nurse %>% 
  filter(num_employees >= 5)
g = nurse5 %>% 
  ggplot(aes(x = year, y = dollar_per_hour,
             colour = hospital_name)) + 
  geom_line() + guides(colour = FALSE) +
  facet_wrap(~ job_title) 

p = ggplotly(g)
p %>% hide_legend()


g_hosp = nurse5 %>% 
  ggplot(aes(x = year, y = dollar_per_hour,
             colour = job_title)) + 
  geom_line() +
  facet_wrap(~ hospital_name) 

p_hosp = ggplotly(g_hosp)
