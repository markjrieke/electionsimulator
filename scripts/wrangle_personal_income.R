# libraries ----
library(tidyverse)

# import ----
f_income <- read_csv("data/raw/SQINC1__ALL_AREAS_1948_2020.csv") 

# helper frames ----

# population frame
f_pop <- 
  read_csv("data/tidy/demographics_1980_2019.csv") %>%
  select(state, year, pop) %>%
  mutate(identity = paste(state, year, sep = "-")) %>%
  select(identity, pop)

# cpi
f_cpi <- 
  read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2021-03-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-04-28&revision_date=2021-04-28&nd=1947-01-01") %>%
  rename(date = DATE,
         cpi = CPIAUCSL)

cpi_bench <-
  f_cpi %>%
  filter(date == max(date)) %>%
  pull(cpi)


# wrangle ----
f_income <- 
  f_income %>%
  select(-GeoFIPS, -Region, -TableName, -LineCode, -IndustryClassification) %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_longer(cols = contains("Q"),
               names_to = "quarter",
               values_to = "val") %>%
  mutate(val = as.numeric(val)) %>%
  filter(GeoName != "United States",
         Description == "Personal income (millions of dollars, seasonally adjusted)") %>%
  rename(state = GeoName) %>%
  mutate(year = str_sub(quarter, 1, 4),
         identity = paste(state, year, sep = "-")) %>%
  group_by(identity) %>%
  mutate(personal_income = mean(val) * 1000000) %>%
  ungroup() %>%
  distinct(identity, .keep_all = TRUE) %>%
  mutate(state = str_remove(state, " \\*")) %>%
  filter(!state %in% c("New England", "Mideast", "Great Lakes", "Plains", 
                       "Southeast", "Southwest", "Rocky Mountain", "Far West")) %>%
  select(state, year, personal_income, identity) %>%
  left_join(f_pop, by = "identity") %>%
  drop_na() %>%
  mutate(inc_per_cap_nom = personal_income/pop,
         date = as.Date(paste(year, "-01-01", sep = ""))) %>%
  left_join(f_cpi, by = "date") %>%
  mutate(pct_mult = cpi_bench / cpi,
         inc_per_cap_act = inc_per_cap_nom * pct_mult) %>%
  select(state, year, inc_per_cap_act)

# write ----
write_csv(f_income, 
          "data/tidy/personal_income_per_cap_act_1980_2019.csv")
