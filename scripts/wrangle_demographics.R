# libraries ----
library(tidyverse)

# 2010 - 2019 ----

# import
f_cb2010_2019 <- read_csv("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/asrh/sc-est2019-alldata6.csv")

# wrangle
f_cb2010_2019 <- 
  f_cb2010_2019 %>% 
  
  # quality of life wrangling
  select(-SUMLEV, -REGION, -DIVISION, -STATE, -CENSUS2010POP, -ESTIMATESBASE2010) %>% 
  pivot_longer(cols = starts_with("POPESTIMATE"),
               names_to = "year",
               names_prefix = "POPESTIMATE",
               values_to = "pop") %>%
  rename(state = NAME,
         sex = SEX,
         origin = ORIGIN,
         race = RACE,
         age = AGE) %>%
  
  # remove totals (represented by 0)
  filter(sex != 0,
         origin != 0) %>%
  
  # determine pct_male
  pivot_wider(names_from = sex,
              values_from = pop) %>%
  rename(male = "1",
         female = "2") %>%
  group_by(state, year) %>%
  mutate(tot_male = sum(male),
         tot_female = sum(female)) %>%
  ungroup(state, year) %>%
  mutate(tot_pop = tot_male + tot_female,
         pct_male = tot_male/tot_pop) %>%
  select(-tot_male, -tot_female, -tot_pop) %>%
  mutate(tot_row = male + female) %>%
  select(-male, -female) %>%
  relocate(tot_row, .after = year) %>%
  arrange(year) %>%
  
  # determine pct_ of each race (white/black/hispanic/other)
  mutate(race = if_else(origin == 1, race, 7)) %>%
  select(-origin) %>%
  group_by(state, race, age, year) %>%
  mutate(tot_row = sum(tot_row)) %>%
  distinct(tot_row, .keep_all = TRUE) %>%
  ungroup(state, race, age, year) %>%
  pivot_wider(names_from = race,
              values_from = tot_row) %>%
  rename(white = "1",
         black = "2",
         nat_american = "3",
         asian = "4",
         pac_island = "5",
         other = "6",
         hispanic = "7") %>%
  mutate(tot_row = white + black + nat_american + asian + pac_island + other + hispanic) %>%
  group_by(state, year) %>%
  mutate(pct_white = sum(white)/sum(tot_row),
         pct_black = sum(black)/sum(tot_row),
         pct_hispanic = sum(hispanic)/sum(tot_row),
         pct_other = (sum(nat_american) + sum(asian) + sum(pac_island) + sum(other))/sum(tot_row)) %>%
  ungroup(state, year) %>%
  select(-white, -black, -nat_american, -asian, -pac_island, -hispanic, -other) %>%
  
  # determine average age and average age of adults-only
  mutate(age_score = age * tot_row) %>%
  group_by(state, year) %>%
  mutate(avg_age = sum(age_score)/sum(tot_row)) %>%
  ungroup(state, year) %>%
  mutate(age_score = if_else(age < 18, 0, age * tot_row),
         age_total = if_else(age < 18, 0, tot_row)) %>%
  group_by(state, year) %>%
  mutate(avg_age_adult = sum(age_score)/sum(age_total)) %>%
  ungroup(state, year) %>%
  select(-age_score, -age_total) %>%
  
  # determine percentage breakdown by age group
  mutate(age_00_17 = if_else(age < 18, tot_row, 0),
         age_18_34 = if_else(age >= 18 & age < 35, tot_row, 0),
         age_35_64 = if_else(age >= 35 & age < 65, tot_row, 0),
         age_65_00 = if_else(age >= 65, tot_row, 0)) %>%
  group_by(state, year) %>%
  mutate(pct_00_17 = sum(age_00_17)/sum(tot_row),
         pct_18_34 = sum(age_18_34)/sum(tot_row),
         pct_35_64 = sum(age_35_64)/sum(tot_row),
         pct_65_00 = sum(age_65_00)/sum(tot_row),
         pop = sum(tot_row)) %>%
  
  # final tidying
  ungroup(state, year) %>%
  group_by(state) %>%
  distinct(year, .keep_all = TRUE) %>%
  ungroup() %>%
  select(-age_00_17, -age_18_34, -age_35_64, -age_65_00, -age, -tot_row) %>%
  relocate(pop, .after = year)

# 2000 - 2009 ----

# import
f_cb2000_2009 <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/state/asrh/sc-est2009-alldata6-all.csv")

# import states file (get state name from FIPS)
f_states2 <- read_csv("data/raw/states2.csv")

# wrangle
f_cb2000_2009 <- 
  f_cb2000_2009 %>%
  
  # quality of life wrangling
  select(-SUMLEV, -REGION, -DIVISION, -CENSUS2000POP, -ESTIMATESBASE2000)  %>%
  left_join(f_states2, by = c("STATE" = "FIPS")) %>%
  select(-STATE, -`Postal Code`) %>%
  relocate(Name) %>%
  pivot_longer(cols = starts_with("POPESTIMATE"),
               names_to = "year",
               names_prefix = "POPESTIMATE",
               values_to = "pop") %>%
  rename(state = Name,
         sex = SEX,
         origin = ORIGIN,
         race = RACE,
         age = AGE) %>%
  
  # remove totals (0 indicates total)
  filter(sex != 0,
         origin != 0,
         state != "United States") %>%
  
  # determine pct_male
  pivot_wider(names_from = sex,
              values_from = pop) %>%
  rename(male = "1",
         female = "2") %>%
  group_by(state, year) %>%
  mutate(tot_male = sum(male),
         tot_female = sum(female)) %>%
  ungroup(state, year) %>%
  mutate(tot_pop = tot_male + tot_female,
         pct_male = tot_male/tot_pop) %>%
  select(-tot_male, -tot_female, -tot_pop) %>%
  mutate(tot_row = male + female) %>%
  select(-male, -female) %>%
  relocate(tot_row, .after = year) %>%
  arrange(year) %>%
  
  # determine pct_ of each race (white/black/hispanic/other)
  mutate(race = if_else(origin == 1, race, 7)) %>%
  select(-origin) %>%
  group_by(state, race, age, year) %>%
  mutate(tot_row = sum(tot_row)) %>%
  distinct(tot_row, .keep_all = TRUE) %>%
  ungroup(state, race, age, year) %>%
  pivot_wider(names_from = race,
              values_from = tot_row) %>%
  rename(white = "1",
         black = "2",
         nat_american = "3",
         asian = "4",
         pac_island = "5",
         other = "6",
         hispanic = "7") %>%
  mutate(tot_row = white + black + nat_american + asian + pac_island + hispanic + other) %>%
  group_by(state, year) %>%
  mutate(pct_white = sum(white)/sum(tot_row),
         pct_black = sum(black)/sum(tot_row), 
         pct_hispanic = sum(hispanic)/sum(tot_row),
         pct_other = (sum(nat_american) + sum(asian) + sum(pac_island) + sum(other))/sum(tot_row)) %>%
  ungroup(state, year) %>%
  select(-white, -black, -nat_american, -asian, -pac_island, -hispanic, -other) %>%
  
  # determine average age and average age adults-only
  mutate(age_score = age * tot_row) %>%
  group_by(state, year) %>%
  mutate(avg_age = sum(age_score)/sum(tot_row)) %>%
  ungroup(state, year) %>%
  mutate(age_score = if_else(age < 18, 0, age * tot_row),
         age_total = if_else(age < 18, 0, tot_row)) %>%
  group_by(state, year) %>%
  mutate(avg_age_adult = sum(age_score)/sum(age_total)) %>%
  ungroup(state, year) %>%
  select(-age_score, -age_total) %>%
  
  # determine pct of each age group
  mutate(age_00_17 = if_else(age < 18, tot_row, 0),
         age_18_34 = if_else(age >= 18 & age < 35, tot_row, 0),
         age_35_64 = if_else(age >= 35 & age < 65, tot_row, 0),
         age_65_00 = if_else(age >= 65, tot_row, 0)) %>%
  group_by(state, year) %>%
  mutate(pct_00_17 = sum(age_00_17)/sum(tot_row),
         pct_18_34 = sum(age_18_34)/sum(tot_row),
         pct_35_64 = sum(age_35_64)/sum(tot_row),
         pct_65_00 = sum(age_65_00)/sum(tot_row),
         pop = sum(tot_row)) %>%
  
  # final tidying
  ungroup(state, year) %>%
  group_by(state) %>%
  distinct(year, .keep_all = TRUE) %>%
  ungroup() %>%
  select(-age_00_17, -age_18_34, -age_35_64, -age_65_00, -age, -tot_row) %>%
  relocate(pop, .after = year)

# 1990 - 1999 ----

# create age groups tibble
f_agegroups <- tibble(group = seq(0, 18, 1),
                      mean_age = c(0, seq(2.5, 82.5, 5), 85))

# import individual years
f_cb1990 <- read_fwf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1990.txt",
                     fwf_empty("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1990.txt",
                               col_names = c("year", "state-county_FIPS", "age_group", "race-sex", "origin", "pop")))

f_cb1991 <- read_fwf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1991.txt",
                     fwf_empty("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1991.txt",
                               col_names = c("year", "state-county_FIPS", "age_group", "race-sex", "origin", "pop")))

f_cb1992 <- read_fwf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1992.txt",
                     fwf_empty("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1992.txt",
                               col_names = c("year", "state-county_FIPS", "age_group", "race-sex", "origin", "pop")))

f_cb1993 <- read_fwf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1993.txt",
                     fwf_empty("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1993.txt",
                               col_names = c("year", "state-county_FIPS", "age_group", "race-sex", "origin", "pop")))

f_cb1994 <- read_fwf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1994.txt",
                     fwf_empty("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1994.txt",
                               col_names = c("year", "state-county_FIPS", "age_group", "race-sex", "origin", "pop")))

f_cb1995 <- read_fwf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1995.txt",
                     fwf_empty("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1995.txt",
                               col_names = c("year", "state-county_FIPS", "age_group", "race-sex", "origin", "pop")))

f_cb1996 <- read_fwf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1996.txt",
                     fwf_empty("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1996.txt",
                               col_names = c("year", "state-county_FIPS", "age_group", "race-sex", "origin", "pop")))

f_cb1997 <- read_fwf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1997.txt",
                     fwf_empty("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1997.txt",
                               col_names = c("year", "state-county_FIPS", "age_group", "race-sex", "origin", "pop")))

f_cb1998 <- read_fwf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1998.txt",
                     fwf_empty("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1998.txt",
                               col_names = c("year", "state-county_FIPS", "age_group", "race-sex", "origin", "pop")))

f_cb1999 <- read_fwf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1999.txt",
                     fwf_empty("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1999.txt",
                               col_names = c("year", "state-county_FIPS", "age_group", "race-sex", "origin", "pop")))

# merge individual years into one large frame
f_cb1990_1999 <- bind_rows(f_cb1990, f_cb1991, f_cb1992, f_cb1993, f_cb1994,
                           f_cb1995, f_cb1996, f_cb1997, f_cb1998, f_cb1999)

# remove superfluous frames
rm(f_cb1990, f_cb1991, f_cb1992, f_cb1993, f_cb1994,
   f_cb1995, f_cb1996, f_cb1997, f_cb1998, f_cb1999)

# add FIPS frame to convert from county FIPS code to state
f_fips <- read_csv("data/raw/county_FIPS.csv")

# add 0 to FIPS code to make left_join work
f_fips <- f_fips %>%
  mutate(FIPS = if_else(FIPS < 10000, 
                        paste("0", as.character(FIPS), sep = ""),
                        as.character(FIPS)),
         Name = if_else(FIPS == 11001,
                        "District of Columbia",
                        Name))

# adding state frame to get state name
f_states <- 
  read_csv("data/raw/states.csv") %>%
  add_row(`US STATE` = "District of Columbia",
          ABBREVIATION = "DC")

# wrangle 
f_cb1990_1999 <- 
  f_cb1990_1999 %>%
  
  # quality of life wrangling
  left_join(f_fips, by = c("state-county_FIPS" = "FIPS")) %>%
  select(-`state-county_FIPS`, -Name) %>%
  relocate(State, .after = year) %>%
  rename(state = State,
         race_sex = `race-sex`) %>%
  mutate(year = as.character(year)) %>%
  
  # creating state summaries from the county data
  group_by(year, state, age_group, race_sex, origin) %>%
  mutate(pop = sum(pop)) %>%
  ungroup(year, state, age_group, race_sex, origin) %>%
  mutate(identity = paste(year, state, age_group, race_sex, origin, sep = "-")) %>%
  distinct(identity, .keep_all = TRUE) %>%
  select(-identity) %>%
  
  # determing pct_male
  pivot_wider(names_from = race_sex,
              values_from = pop) %>%
  rename(white_male = "1",
         white_female = "2",
         black_male = "3",
         black_female = "4",
         nat_amer_male = "5",
         nat_amer_female = "6",
         aapi_male = "7",
         aapi_female = "8") %>%
  mutate(male = white_male + black_male + nat_amer_male + aapi_male,
         female = white_female + black_female + nat_amer_female + aapi_female,
         white = white_male + white_female,
         black = black_male + black_female,
         nat_amer = nat_amer_male + nat_amer_female,
         aapi = aapi_male + aapi_female) %>%
  select(-white_male, -black_male, -nat_amer_male, -aapi_male, 
         -white_female, -black_female, -nat_amer_female, -aapi_female) %>%
  group_by(year, state) %>%
  mutate(pct_male = sum(male)/(sum(male) + sum(female))) %>%
  ungroup(year, state) %>%
  select(-male, -female) %>%
  
  # determine pct of each race (white/black/hispanic/other)
  pivot_longer(cols = c("white", "black", "nat_amer", "aapi"),
               names_to = "race",
               values_to = "pop") %>%
  mutate(race = if_else(origin == 2, "hispanic", race)) %>%
  select(-origin) %>%
  group_by(year, state, age_group, race) %>%
  mutate(tot_pop = sum(pop),
         identity = paste(year, state, age_group, race, sep = "-")) %>%
  ungroup(year, state, age_group, race) %>%
  distinct(identity, .keep_all = TRUE) %>%
  select(-identity, -pop) %>%
  pivot_wider(names_from = race,
              values_from = tot_pop) %>%
  drop_na() %>%
  mutate(tot_row = white + black + nat_amer + aapi + hispanic) %>%
  group_by(year, state) %>%
  mutate(pct_white = sum(white)/sum(tot_row),
         pct_black = sum(black)/sum(tot_row),
         pct_hispanic = sum(hispanic)/sum(tot_row),
         pct_other = (sum(nat_amer) + sum(aapi))/sum(tot_row)) %>%
  ungroup(year, state) %>%
  select(-white, -black, -nat_amer, -aapi, -hispanic) %>%
  
  # determine avg age and avg age of adults
  left_join(f_agegroups, by = c("age_group" = "group")) %>%
  relocate(mean_age, .after = age_group) %>%
  mutate(age_score = mean_age * tot_row,
         age_score_adult = if_else(age_group < 4,
                                   0,
                                   if_else(age_group == 4, 
                                           0.4 * 19 * tot_row,
                                           mean_age * tot_row)),
         tot_adult = if_else(age_group < 4, 
                             0,
                             if_else(age_group == 4,
                                     0.4 * tot_row,
                                     tot_row))) %>%
  group_by(year, state) %>%
  mutate(avg_age = sum(age_score)/sum(tot_row),
         avg_age_adult = sum(age_score_adult)/sum(tot_adult)) %>%
  ungroup(year, state) %>%
  select(-age_score, -age_score_adult, -tot_adult) %>%
  
  # determine pct of each age group
  mutate(age_00_17 = if_else(age_group < 4,
                             tot_row,
                             if_else(age_group == 4,
                                     tot_row * 0.6,
                                     0)),
         age_18_34 = if_else(age_group < 4, 
                             0,
                             if_else(age_group == 4,
                                     tot_row * 0.4,
                                     if_else(age_group <= 7,
                                             tot_row,
                                             0))),
         age_35_64 = if_else(age_group < 8,
                             0,
                             if_else(age_group > 13,
                                     0,
                                     tot_row)),
         age_65_00 = if_else(age_group < 14,
                             0,
                             tot_row)) %>%
  group_by(year, state) %>%
  mutate(pct_00_17 = sum(age_00_17)/sum(tot_row),
         pct_18_34 = sum(age_18_34)/sum(tot_row),
         pct_35_64 = sum(age_35_64)/sum(tot_row),
         pct_65_00 = sum(age_65_00)/sum(tot_row),
         pop = sum(tot_row)) %>%
  ungroup(year, state) %>%
  select(-age_00_17, -age_18_34, -age_35_64, -age_65_00, -age_group, -mean_age, -tot_row) %>%
  
  # final tidying
  mutate(identity = paste(state, year, sep = "-")) %>%
  distinct(identity, .keep_all = TRUE) %>%
  mutate(year = paste("19", year, sep = "")) %>%
  left_join(f_states, by = c("state" = "ABBREVIATION")) %>%
  mutate(state = `US STATE`) %>%
  select(-`US STATE`, -identity) %>%
  relocate(state) %>%
  relocate(pop, .after = year)

# 1981 - 1989 ----

# create a new age group frame specific to the 80s
f_agegroups2 <- tibble(age_group = c("age_00_04", "age_05_09", "age_10_14",
                                     "age_15_19", "age_20_24", "age_25_29",
                                     "age_30_34", "age_35_39", "age_40_44",
                                     "age_45_49", "age_50_54", "age_55_59",
                                     "age_60_64", "age_65_69", "age_70_74",
                                     "age_75_79", "age_80_84", "age_85_00"),
                       mean_age = c(seq(2.5, 82.5, 5), 85))

# import
f_cb1981_1989 <- read_fwf("https://www2.census.gov/programs-surveys/popest/datasets/1980-1990/state/asrh/st_int_asrh.txt",
                          fwf_empty("https://www2.census.gov/programs-surveys/popest/datasets/1980-1990/state/asrh/st_int_asrh.txt",
                                    col_names = c("state_year_race_sex", 
                                                  "age_00_04", "age_05_09", "age_10_14", 
                                                  "age_15_19", "age_20_24", "age_25_29", 
                                                  "age_30_34", "age_35_39", "age_40_44", 
                                                  "age_45_49", "age_50_54", "age_55_59", 
                                                  "age_60_64", "age_65_69", "age_70_74", 
                                                  "age_75_79", "age_80_84", "age_85_00")))

# wrangle
f_cb1981_1989 <- 
  f_cb1981_1989 %>%
  
  # quality of life wrangling
  mutate(state_FIPS = str_sub(state_year_race_sex, 1, 2),
         year = str_sub(state_year_race_sex, 3, 3),
         race = str_sub(state_year_race_sex, 4, 4),
         sex = str_sub(state_year_race_sex, 5, 5),
         age_45_49 = as.numeric(age_45_49),
         age_50_54 = as.numeric(age_50_54),
         age_55_59 = as.numeric(age_55_59),
         age_60_64 = as.numeric(age_60_64),
         age_65_69 = as.numeric(age_65_69),
         age_80_84 = as.numeric(age_80_84),
         year = paste("198", year, sep = "")) %>%
  select(-state_year_race_sex) %>%
  pivot_longer(cols = starts_with("age"),
               names_to = "age_group",
               values_to = "pop") %>%
  
  # determine pct_male
  pivot_wider(names_from = sex,
              values_from = pop) %>%
  rename(male = "1",
         female = "2") %>%
  mutate(tot_row = male + female) %>%
  group_by(state_FIPS, year) %>%
  mutate(pct_male = sum(male)/sum(tot_row)) %>%
  ungroup(state_FIPS, year) %>%
  select(-male, -female) %>%
  mutate(race = as.numeric(race),
         race = if_else(race > 4, "hispanic", as.character(race))) %>%
  mutate(identity = paste(state_FIPS, year, race, age_group, sep = "-")) %>%
  group_by(identity) %>%
  mutate(tot_row = sum(tot_row)) %>%
  ungroup() %>%
  distinct(identity, .keep_all = TRUE) %>%
  select(-identity) %>%
  
  # determine pct of each race (white/black/hispanic/other)
  pivot_wider(names_from = race,
              values_from = tot_row) %>%
  rename(white = "1",
         black = "2",
         nat_american = "3",
         aapi = "4") %>%
  mutate(tot_row = white + black + nat_american + aapi + hispanic) %>%
  group_by(state_FIPS, year) %>%
  mutate(pct_white = sum(white)/sum(tot_row),
         pct_black = sum(black)/sum(tot_row),
         pct_hispanic = sum(hispanic)/sum(tot_row),
         pct_other = (sum(nat_american) + sum(aapi))/sum(tot_row)) %>%
  ungroup(state_FIPS, year) %>%
  select(-white, -black, -nat_american, -aapi, -hispanic) %>%
  
  # determine avg age and avg age of adults
  left_join(f_agegroups2, by = "age_group") %>%
  relocate(mean_age, .after = age_group) %>%
  mutate(age_score = mean_age * tot_row,
         age_score_adult = if_else(mean_age < 17.5,
                                   0,
                                   if_else(mean_age == 17.5,
                                           0.4 * mean_age * tot_row,
                                           mean_age * tot_row)),
         tot_row_adult = if_else(mean_age < 17.5,
                                 0,
                                 if_else(mean_age == 17.5,
                                         0.4 * tot_row,
                                         tot_row))) %>%
  group_by(state_FIPS, year) %>%
  mutate(avg_age = sum(age_score)/sum(tot_row),
         avg_age_adult = sum(age_score_adult)/sum(tot_row_adult)) %>%
  ungroup(state_FIPS, year) %>%
  select(-age_score, -age_score_adult, -tot_row_adult) %>%
  
  # determine pct of each age group
  mutate(age_00_17 = if_else(mean_age < 17.5,
                             tot_row,
                             if_else(mean_age == 17.5,
                                     0.6 * tot_row,
                                     0)),
         age_18_34 = if_else(mean_age < 17.5,
                             0,
                             if_else(mean_age == 17.5,
                                     0.4 * tot_row,
                                     if_else(mean_age < 35,
                                             tot_row,
                                             0))),
         age_35_64 = if_else(mean_age < 35,
                             0,
                             if_else(mean_age < 65,
                                     tot_row,
                                     0)),
         age_65_00 = if_else(mean_age < 65,
                             0,
                             tot_row)) %>%
  group_by(state_FIPS, year) %>%
  mutate(pct_00_17 = sum(age_00_17)/sum(tot_row),
         pct_18_34 = sum(age_18_34)/sum(tot_row),
         pct_35_64 = sum(age_35_64)/sum(tot_row),
         pct_65_00 = sum(age_65_00)/sum(tot_row),
         pop = sum(tot_row),
         identity = paste(state_FIPS, year, sep = "-")) %>%
  
  # final tidying
  ungroup(state_FIPS, year) %>%
  distinct(identity, .keep_all = TRUE) %>%
  select(-age_group, -mean_age, -tot_row, -age_00_17, 
         -age_18_34, -age_35_64, -age_65_00, -identity) %>%
  left_join(f_states2, by = c("state_FIPS" = "FIPS")) %>%
  select(-state_FIPS, -`Postal Code`) %>%
  rename(state = Name) %>%
  relocate(state) %>%
  relocate(pop, .after = year)

# 1980 ----

# import
f_cb1980 <- read_csv("data/raw/nhgis0003.csv")

# wrangle
f_cb1980 <- 
  f_cb1980 %>%
  
  # quality of life wrangling
  select(-GISJOIN, -STATEFP, -STATENH, -NAME) %>%
  rename(year = YEAR,
         state = STATE,
         male = A08AA,
         female = A08AB,
         age_00_04 = B57AA,
         age_05_09 = B57AB,
         age_10_14 = B57AC,
         age_15_17 = B57AD,
         age_18_19 = B57AE,
         age_20 = B57AF,
         age_21 = B57AG,
         age_22_24 = B57AH,
         age_25_29 = B57AI,
         age_30_34 = B57AJ,
         age_35_44 = B57AK,
         age_45_54 = B57AL,
         age_55_59 = B57AM,
         age_60_61 = B57AN,
         age_62_64 = B57AO,
         age_65_74 = B57AP,
         age_75_84 = B57AQ,
         age_85_00 = B57AR,
         white = AE7AA,
         black = AE7AB,
         nat_american = AE7AC,
         aapi = AE7AD,
         other = AE7AE,
         mixed = AE7AF,
         white_hispanic = AE7AG,
         black_hispanic = AE7AH,
         nat_american_hispanic = AE7AI,
         aapi_hispanic = AE7AJ,
         other_hispanic = AE7AK,
         mixed_hispanic = AE7AL) %>%
  mutate(year = as.character(year)) %>%
  filter(year == "1980") %>%
  
  # determine pct_male
  mutate(pop = male + female,
         pct_male = male/pop) %>%
  
  # determine pct of each race (white/black/hispanic/other [mixed was all NAs])
  select(-male, -female, -mixed, -mixed_hispanic) %>%
  mutate(pct_white = white/pop,
         pct_black = black/pop,
         pct_hispanic = (white_hispanic + black_hispanic + nat_american_hispanic +
                           aapi_hispanic + other_hispanic)/pop,
         pct_other = (nat_american + aapi + other)/pop) %>%
  select(-white, -black, -nat_american, -aapi, -other, -white_hispanic, 
         -black_hispanic, -nat_american_hispanic, -aapi_hispanic, -other_hispanic) %>%
  
  # determine avg age, avg adult age, and pct of each age group
  mutate(age_score = 2.5 * age_00_04 + 7.5 * age_05_09 + 12.5 * age_10_14 +
           16.5 * age_15_17 + 19 * age_18_19 + 20 * age_20 + 21 * age_21 + 
           23.5 * age_22_24 + 27.5 * age_25_29 + 32.5 * age_30_34 + 40 * age_35_44 +
           50 * age_45_54 + 57.5 * age_55_59 + 61 * age_60_61 + 63.5 * age_62_64 + 
           70 * age_65_74 + 80 * age_75_84 + 85 * age_85_00,
         avg_age = age_score/pop,
         age_score_adult = 19 * age_18_19 + 20 * age_20 + 21 * age_21 + 
           23.5 * age_22_24 + 27.5 * age_25_29 + 32.5 * age_30_34 + 40 * age_35_44 +
           50 * age_45_54 + 57.5 * age_55_59 + 61 * age_60_61 + 63.5 * age_62_64 + 
           70 * age_65_74 + 80 * age_75_84 + 85 * age_85_00,
         tot_pop_adult = age_18_19 + age_20 + age_21 + age_22_24 + age_25_29 + 
           age_30_34 + age_35_44 + age_45_54 + age_55_59 + age_60_61 + age_62_64 +
           age_65_74 + age_75_84 + age_85_00,
         avg_age_adult = age_score_adult/tot_pop_adult,
         age_00_17 = age_00_04 + age_05_09 + age_10_14 + age_15_17,
         age_18_34 = age_18_19 + age_20 + age_21 + age_22_24 + age_25_29 + age_30_34,
         age_35_64 = age_35_44 + age_45_54 + age_55_59 + age_60_61 + age_62_64,
         age_65_00 = age_65_74 + age_75_84 + age_85_00,
         pct_00_17 = age_00_17/pop,
         pct_18_34 = age_18_34/pop,
         pct_35_64 = age_35_64/pop,
         pct_65_00 = age_65_00/pop) %>%
  
  # final tidying
  relocate(state) %>%
  relocate(pop, .after = year) %>%
  select(state, year, pop, pct_male, pct_white, pct_black,
         pct_hispanic, pct_other, avg_age, avg_age_adult,
         pct_00_17, pct_18_34, pct_35_64, pct_65_00)

# final frame tidy and write ----

# merge together
f_cb1980_2019 <- 
  bind_rows(f_cb1980,
            f_cb1981_1989,
            f_cb1990_1999,
            f_cb2000_2009,
            f_cb2010_2019)

# write to file
f_cb1980_2019 %>%
  write_csv("data/tidy/deomographics_1980_2019.csv")

# clean
rm(list = ls())

