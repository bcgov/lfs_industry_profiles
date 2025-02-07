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

tictoc::tic()
#functions---------------------
source(here::here("R", "functions.R"))
# detach packages (for some reason??? running script more than once caused problems (think it has to do with the order of library loading))--------------
lapply(names(sessionInfo()$otherPkgs), detach_package)

#attach packages----------------
required_packages <- c("tidyverse",
                       "lubridate",
                       "readxl",
                       "XLConnect",
                       "scales",
                       "fabletools",
                       "fracdiff",
                       "urca",
                       "feasts",
                       "conflicted",
                       "here"
                       )
names(required_packages) <- required_packages
lapply(required_packages, load_package)
conflicts_prefer(dplyr::filter)

# constants---------------
#ma_months <- 3 #how many months to use for smoothing the data
accuracy_large <- 100 #levels rounded to nearest hundred
accuracy_small <- .1 #percentages rounded to nearest tenth

mapping <- read_excel(here("data","industry_mapping_2025.xlsx"))|>
  select(naics_5, contains("industry_profile"))

agg <- mapping|>
  select(-naics_5)|>
  pivot_longer(cols=everything(), names_to = "agg_level", values_to = "industry")|>
  mutate(agg_level=word(agg_level, -1, sep = "_"))|>
  na.omit()|>#could be a mistake
  distinct()#could be a mistake

formatting <- agg|>
  pivot_wider(names_from = agg_level, values_from = agg_level)|>
  mutate(industry=factor(industry, levels = industry, ordered = TRUE))

high_parents <- mapping|>
  select(parent=industry_profile_high, industry=industry_profile_high)|>
  distinct()

medium_parents <- mapping|>
  select(parent=industry_profile_high, industry=industry_profile_medium)|>
  distinct()

low_parents <- mapping|>
  select(parent=industry_profile_high, industry=industry_profile_low)|>
  distinct()

parents <- bind_rows(high_parents, medium_parents, low_parents)|>
  distinct()

# read in the data and join with mapping file to get aggregation info -------------------
ftpt <- read_naics("ftptemp4digNAICS", ftpt)%>%
  inner_join(mapping, by = "naics_5")

status <- read_naics("lfsstat4digNAICS", lf_stat)%>%
  inner_join(mapping, by = "naics_5")
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
high_agg <- agg_level(truncated, industry_profile_high)
medium_agg <- agg_level(truncated, industry_profile_medium)
low_agg <- agg_level(truncated, industry_profile_low)

# bind the 3 levels of aggregation together
all_data <- bind_rows(high_agg, medium_agg, low_agg)%>%
  na.omit() %>%
  mutate(data = map(data, add_vars)) #add in labour force and unemployment rate

high_and_medium <- bind_rows(high_agg, medium_agg)%>%
  na.omit() %>%
  mutate(data = map(data, add_vars)) #add in labour force and unemployment rate

all_with_mapping <- full_join(all_data, agg)%>%
  mutate(data=map(data, na.omit))%>%
  unnest(data)%>%
  group_by(agg_level, name)%>%
  nest()%>%
  mutate(name=str_to_title(str_replace_all(name, "_", " ")),
         data=map(data, distinct),
         data=map(data, pivot_wider, names_from="date", values_from="value"))

write_rds(all_with_mapping, here::here("temp","all_with_mapping.rds"))

for_ts_plots <- high_and_medium|>
  left_join(parents)|>
  unnest(data)

write_rds(for_ts_plots, here::here("temp","for_ts_plots.rds"))

keep_list <- c("industry",
                 "trend_strength",
                 "seasonal_strength_year",
                 "spikiness",
                 "linearity",
                 "curvature",
                 "shift_level_max",
                 "spectral_entropy",
                 "coef_hurst"
  )

  for_pca <- all_with_mapping %>%
    unnest(data)%>%
    pivot_longer(cols=-c(agg_level, name, industry), names_to = "date", values_to = "value")%>%
    filter(agg_level=="high")%>%
    ungroup()|>
    select(-agg_level)|>
    ungroup()%>%
    mutate(date=tsibble::yearmonth(date))%>%
    group_by(name)%>%
    nest()%>%
    mutate(data=map(data, tsibble::tsibble, key=industry, index=date),
           features=map(data, function(tsbbl) tsbbl %>% features(value, feature_set(pkgs = "feasts"))),
           features=map(features, select, all_of(keep_list)),
           features=map(features, column_to_rownames, var="industry"),
           features=map(features, fix_column_names),
           pcs=map(features, prcomp, scale=TRUE)
    )

  write_rds(for_pca, here::here("temp","for_pca.rds"))

no_format <- all_data %>%
  mutate(current = map(data, get_values, 0), # get current value
        last_month = map(data, get_values, 1),
        last_year = map(data, get_values, 12),
        current_ytd_ave = map(data, ytd_ave, 0), # year to date average
        previous_ytd_ave = map(data, ytd_ave, 1)
  )%>%
  select(-data)%>%
  mutate(#join all the dataframes created above for unnesting below (unnesting individually creates sparse dataframe)
    data = map2(current, last_month, full_join, by = "name"),
    data = map2(data, last_year, full_join, by = "name"),
    data = map2(data, current_ytd_ave, full_join, by = "name"),
    data = map2(data, previous_ytd_ave, full_join, by = "name")
  ) %>%
  select(industry, data) %>%
  unnest(data)%>%
  dplyr::rename(
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


for_plots <- left_join(no_format, agg)%>%
  ungroup()%>%
  group_by(industry, name)%>%
  nest()%>%
  mutate(name=str_to_title(str_replace_all(name, "_", " ")))

write_rds(for_plots, here::here("temp","for_plots.rds"))

# formatting the output for excel
with_formatting <- no_format%>%
  mutate(percent_change_year = percent(percent_change_year, accuracy = accuracy_small),
        percent_change_month = percent(percent_change_month, accuracy = accuracy_small),
        percent_change_ytd = percent(percent_change_ytd, accuracy = accuracy_small),
        current = case_when(name=="unemployment_rate" ~ percent(current, accuracy = accuracy_small),
                            current < 1500 ~ "suppressed",
                            TRUE ~ comma(current, accuracy = accuracy_large)),
        previous_year=case_when(name=="unemployment_rate" ~ percent(previous_year, accuracy = accuracy_small),
                                previous_year<1500 ~ "suppressed",
                                TRUE ~ comma(previous_year, accuracy = accuracy_large)),
        previous_month=case_when(name=="unemployment_rate" ~ percent(previous_month, accuracy = accuracy_small),
                                previous_month<1500 ~ "suppressed",
                                TRUE ~ comma(previous_month, accuracy = accuracy_large)),
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
      )|>
  inner_join(formatting)|>
  mutate(industry=factor(industry, levels=levels(formatting$industry), ordered=TRUE))|>
  arrange(industry)|>
  left_join(parents)|>
  group_by(parent)|>
  nest()|>
  mutate(data=map(data, apply_formatting),
         data=map(data, clean_up))

write_rds(with_formatting, here::here("temp","for_tables.rds"))

# write to excel-----------------
wb <- loadWorkbook(here::here("data", "template.xlsx")) # get the desired sheet header
createSheet(wb, name = "Mapping")
setColumnWidth(wb, sheet = "Mapping", column = 2:4, width = c(16000, 24000, 16000))
writeWorksheet(wb, mapping, sheet="Mapping")
with_formatting%>%
  mutate(walk2(data, parent, write_sheet)) # replicates the template sheet and writes data to each sheet
removeSheet(wb, "layout") # get rid of the template
saveWorkbook(wb, here::here("out", "current", paste0("LFS_industry_profiles",lubridate::today(),".xlsx")))
tictoc::toc()

