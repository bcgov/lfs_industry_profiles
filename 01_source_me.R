# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' TO DO before sourcing this file:
#' delete old RTRA files from directory "data"
#' upload the 4 .SAS files to https://www75.statcan.gc.ca/eft-tef/en/operations (To StatCan)
#' download the 4 resulting csv files (with names containing ftptemp4digNAICS and lfsstat4digNAICS) and place in directory "data"

#' Note that Jan 2026 the 201620 SAS files will need to be updated to 202630.
tictoc::tic()
# libraries--------------
library(tidyverse)
library(lubridate)
library(readxl)
library(XLConnect)
library(scales)
#library(tsibble) #these 3 libraries only needed if doing stl decomp
#library(feasts)
#library(fable)
# constants---------------
ma_months <- 3 #how many months to use for smoothing the data
accuracy_large <- 100 #levels rounded to nearest hundred
accuracy_small <- .1 #percentages rounded to nearest tenth
# Functions--------------------
source(here::here("R", "functions.R"))
# Start by creating a mapping file from naics to various levels of aggregation----------------
# input file mapping.xlsx uses leading spaces to indicate hierarchy....
raw_mapping <- read_excel(here::here("data", "mapping.xlsx"), trim_ws = FALSE) %>%
  janitor::clean_names() %>%
  mutate(
    spaces = str_count(industry, "\\G "),
    agg = case_when(
      spaces == 0 ~ "high",
      spaces %in% 2:4 ~ "medium",
      spaces %in% 5:6 ~ "low"
    ),
    industry = trimws(industry)
  )
#relationship between each industry and the three levels of aggregation (used for excel layout)------------
agg <- raw_mapping %>%
  select(-naics) %>%
  mutate(
    high = if_else(agg == "high", industry, NA_character_),
    medium = if_else(agg %in% c("high", "medium"), industry, NA_character_),
    low = if_else(agg == "low", industry, NA_character_)
  ) %>%
  fill(high, .direction = "down") %>%
  fill(medium, .direction = "down") %>%
  select(industry, high, medium, low)

write_csv(agg, here::here("out","layout.csv"))


# get the naics for the lowest level of aggregation---------------
low <- raw_mapping %>%
  filter(agg == "low") %>%
  select(low = industry,
         naics) %>%
  group_by(low) %>%
  nest()%>%
  mutate(
    data = map(data, separate_naics),
    data = map(data, fill_wrapper)
  ) %>%
  unnest(data)%>%
  unnest(naics)

# get the naics for the medium level of aggregation---------------
medium <- raw_mapping %>%
  filter(agg == "medium") %>%
  select(medium = industry, naics) %>%
  group_by(medium) %>%
  nest() %>%
  mutate(
    data = map(data, separate_naics),
    data = map(data, fill_wrapper)
  ) %>%
  unnest(data) %>%
  unnest(naics)

# get the naics for the high level of aggregation---------------
high <- raw_mapping %>%
  filter(agg == "high") %>%
  select(high = industry,
         naics) %>%
  group_by(high) %>%
  nest() %>%
  mutate(
    data = map(data, separate_naics),
    data = map(data, fill_wrapper)
  ) %>%
  unnest(data) %>%
  unnest(naics)
# join by naics to get the mapping file from naics to the 3 levels of aggregation.
mapping <- high%>%
  full_join(medium, by="naics")%>%
  full_join(low, by= "naics")%>%
  select(naics, everything())

write_csv(mapping, here::here("out","mapping.csv"))

# read in the data and join with mapping file to get aggregation info -------------------
ftpt <- read_naics("ftptemp4digNAICS", ftpt)%>%
  inner_join(mapping, by = "naics")
status <- read_naics("lfsstat4digNAICS", lf_stat)%>%
  inner_join(mapping, by = "naics")
all_data <- bind_rows(ftpt, status)

#data is zero padded to the end of the current year... figure out the last month from data.
max_date <- all_data%>%
  group_by(date, name)%>%
  summarize(value=mean(value))%>%
  ungroup()%>%
  filter(!near(value, 0))%>%
  filter(date==max(date))%>%
  pull(date)%>%
  unique()

truncated <- all_data%>%
  filter(date <= max_date)

# output file has dates as column headings... get the necessary dates-----------
current <- format(max(truncated$date), "%b-%y")
previous_month <- format(max(truncated$date) - months(1), "%b-%y")
previous_year <- format(max(truncated$date) - years(1), "%b-%y")
# aggregate the data to the three levels-------------
high_agg <- agg_level(truncated, high)
medium_agg <- agg_level(truncated, medium)
low_agg <- agg_level(truncated, low)

# bind the 3 levels of aggregation together then...
smoothed_data <- bind_rows(high_agg, medium_agg, low_agg)%>%
  na.omit() %>%
  mutate(#data = map(data, stl_smooth), #thought this might be better than simple moving average...
        data = map(data, trail_ma, months = ma_months), # simple moving average smooth of data
        data = map(data, add_vars)) #add in labour force and unemployment rate

full_join(smoothed_data, agg, by=c("agg_level"="industry"))%>%
  mutate(data=map(data, na.omit))%>%
  unnest(data)%>%
  group_by(agg_level, high, medium, low, name)%>%
  nest()%>%
  mutate(name=str_to_title(str_replace_all(name, "_", " ")),
         data=map(data, pivot_wider, names_from="date", values_from="value"))%>%
  write_rds(here::here("out","smoothed_with_mapping.rds"))

no_format <- smoothed_data %>%
  mutate(current = map(data, get_smoothed, 0), # get current value of smoothed data
        last_month = map(data, get_smoothed, 1),
        last_year = map(data, get_smoothed, 12),
        current_ytd_ave = map(data, ytd_ave, 0), # year to date average of smoothed data
        previous_ytd_ave = map(data, ytd_ave, 1)
  )%>%
  select(-data)%>%
  mutate(#join all the dataframes created above for unnesting below (unnesting individually creates sparse dataframe)
    data = map2(current, last_month, full_join, by = "name"),
    data = map2(data, last_year, full_join, by = "name"),
    data = map2(data, current_ytd_ave, full_join, by = "name"),
    data = map2(data, previous_ytd_ave, full_join, by = "name")
  ) %>%
  select(agg_level, data) %>%
  unnest(data)%>%
  rename(
    current = value.x, # fix the names messed up by joins above
    previous_month = value.y,
    previous_year = value,
    current_ytd_average = ytd_ave.x,
    previous_ytd_average = ytd_ave.y
  )%>%
  mutate(#create some variables
    level_change_year = current - previous_year,
    level_change_month = current - previous_month,
    level_change_ytd = current_ytd_average - previous_ytd_average,
    percent_change_year = level_change_year / previous_year,
    percent_change_month = level_change_month / previous_month,
    percent_change_ytd = level_change_ytd / previous_ytd_average)


full_join(no_format, agg, by=c("agg_level"="industry"))%>%
  ungroup()%>%
  group_by(agg_level, high, medium, low, name)%>%
  nest()%>%
  mutate(name=str_to_title(str_replace_all(name, "_", " ")))%>%
write_rds(here::here("out","for_heatmaps.rds"))

# formatting the output for excel
with_formatting <- no_format%>%
  mutate(percent_change_year = percent(percent_change_year, accuracy = accuracy_small),
        percent_change_month = percent(percent_change_month, accuracy = accuracy_small),
        percent_change_ytd = percent(percent_change_ytd, accuracy = accuracy_small),
        current = if_else(name == "unemployment_rate",
                          percent(current, accuracy = accuracy_small),
                          comma(current, accuracy = accuracy_large)),
        previous_year = if_else(name == "unemployment_rate",
                                percent(previous_year, accuracy = accuracy_small),
                                comma(previous_year, accuracy = accuracy_large)),
        previous_month = if_else(name == "unemployment_rate",
                                 percent(previous_month, accuracy = accuracy_small),
                                 comma(previous_month, accuracy = accuracy_large)),
        level_change_year = if_else(name == "unemployment_rate",
                                    percent(level_change_year, accuracy = accuracy_small),
                                    comma(level_change_year, accuracy = accuracy_large)),
        level_change_month = if_else(name == "unemployment_rate",
                                     percent(level_change_month, accuracy = accuracy_small),
                                     comma(level_change_month, accuracy = accuracy_large)),
        level_change_ytd = if_else(name == "unemployment_rate",
                                   percent(level_change_ytd, accuracy = accuracy_small),
                                   comma(level_change_ytd, accuracy = accuracy_large)),
        current_ytd_average = if_else(name == "unemployment_rate",
                                      percent(current_ytd_average, accuracy = accuracy_small),
                                      comma(current_ytd_average, accuracy = accuracy_large)),
        previous_ytd_average = if_else(name == "unemployment_rate",
                                       percent(previous_ytd_average, accuracy = accuracy_small),
                                       comma(previous_ytd_average, accuracy = accuracy_large))
      ) %>%
  left_join(agg, by = c("agg_level" = "industry"))%>% #agg is the mapping from industry to the 3 levels of aggregation
  mutate(medium = ifelse(agg_level == high, paste0("1", medium), medium)) %>%# allows high level industries to be at top of sorted medium industries.
  group_by(high) %>%
  nest() %>%
  mutate(
    data = map(data, arrange, name, medium), # arranges data by medium level of aggregation (except high level at top because of pasted 1)
    data = map(data, unfill_var, name), # replaces fixed values with blanks. (excel formatting)
    data = map(data, indent_industry), # indents industry to indicate hierarchy.
    data = map(data, select, -medium, -low), # gets rid of aggregation levels
    data = map(data, clean_up) # assigns the desired column names and puts in the correct order
  ) %>%
  filter(!is.na(high))

write_rds(with_formatting, here::here("out","for_tables.rds"))

# write to excel-----------------
wb <- loadWorkbook(here::here("data", "template.xlsx")) # get the desired sheet header
with_formatting%>%
  mutate(walk2(data, high, write_sheet)) # replicates the template sheet and writes data to each sheet
removeSheet(wb, "layout") # get rid of the template
saveWorkbook(wb, here::here("out", "industry_snapshots.xlsx"))#write to file
tictoc::toc()

