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

#' Pre-processing Naics e.g.  1111-1121, 1200, 1220-1300, replace - with : and then separate by comma, return tibble.
separate_naics <- function(my_string) {
  vec <- my_string %>%
    str_replace_all("-", ":") %>%
    str_replace_all(" ", "") %>%
    str_split(",") %>%
    unlist()
  tibble("naics" = vec)
}
#' e.g. inputs 1111:1121 and 1200 return c(1111,1112,1113,....) and c(1200)
fill_ranges <- function(rng) {
  eval(parse(text = rng))
}
#' wrapper function for fill_ranges
fill_wrapper <- function(tbbl) {
  tbbl %>%
    mutate(naics = map(naics, fill_ranges))
}
# function to read in the employment data by naics
read_naics <- function(pttrn, var) {
  file_names <- list.files(here::here("data"), pattern = pttrn)
  vroom::vroom(here::here("data", file_names)) %>%
    janitor::clean_names() %>%
    rename(
      name = {{ var }},
      value = count
    ) %>%
    na.omit()%>%
    filter(naics_5 != "Unknown") %>%
    mutate(
      naics= as.numeric(str_sub(naics_5, start=-4)),
      date = lubridate::ymd(paste(syear, smth, "01", sep = "/"))
    ) %>%
     select(-syear, -smth, -naics_5)
}
# aggregates data to level var for each name and date
agg_level <- function(tbbl, var) {
  tbbl %>%
    group_by({{ var }}, name, date) %>%
    summarize(value = sum(value, na.rm=TRUE)) %>%
    group_by({{ var }}, .add = FALSE) %>%
    nest() %>%
    rename(agg_level = {{ var }})
}
# smooths data over previous months
trail_ma <- function(tbbl, months) {
  tbbl %>%
    group_by(name) %>%
    mutate(value = RcppRoll::roll_meanr(value, n = months)+1) #to avoid avoid infinite values
}
# get the smoothed values for period max(date)-months
get_smoothed <- function(tbbl, num_months) {
  tbbl %>%
    group_by(name) %>%
    filter(near(date, max(date) - months(num_months), tol = 7)) %>%
    select(value)
}
# calculates the ytd average of the smoothed values
ytd_ave <- function(tbbl, num_years) {
  start <- floor_date(max(tbbl$date), unit = "year") - years(num_years)
  end <- max(tbbl$date) - years(num_years)
  tbbl %>%
    group_by(name) %>%
    filter(date >= start & date <= end) %>%
    summarize(ytd_ave = mean(value))
}
# labour force and unemployment rates need to be calculated
add_vars <- function(tbbl) {
  tbbl %>%
    pivot_wider(names_from = name, values_from = value) %>%
    mutate(
      labour_force = Employed + Unemployed,
      unemployment_rate = Unemployed / labour_force
    ) %>%
    select(-Unknown)%>%
    pivot_longer(cols = -date, names_to = "name", values_to = "value")
}
# clears out constant values of var (for excel)
unfill_var <- function(tbbl, var) {
  tbbl %>%
    mutate(characteristic = {{ var }} == dplyr::lag({{ var }}), .before = 1) %>%
    mutate(characteristic = if_else(!is.na(characteristic) & characteristic, "", {{ var }})) %>%
    select(-{{ var }})
}
# indicate hierarchy of industries by indentation
indent_industry <- function(tbbl) {
  tbbl %>%
    mutate(
      agg_level = if_else(characteristic == "", paste0("     ", agg_level), agg_level),
      agg_level = if_else(is.na(low), agg_level, paste0("     ", agg_level))
    )
}
# puts columns in the correct order and gives them the correct names
clean_up <- function(tbbl) {
  tbbl %>%
    mutate(characteristic = str_to_title(str_replace_all(characteristic, "_", " "))) %>%
    select(
      Characteristic = characteristic,
      Industry = agg_level,
      "{previous_year}" := previous_year,
      "{previous_month}" := previous_month,
      "{current}" := current,
      "Jan-{previous_year}" := previous_ytd_average,
      "Jan-{current}" := current_ytd_average,
      `Y/Y` = level_change_year,
      `M/M` = level_change_month,
      `YTD/YTD` = level_change_ytd,
      `Y/Y%` = percent_change_year,
      `M/M%` = percent_change_month,
      `YTD/YTD%` = percent_change_ytd
    )
}
# clones the layout sheet of the template to new sheets with the industry names
write_sheet <- function(tbbl, long_name) {
  sheet_name <- str_trunc(long_name, width = 31) # excel cant handle sheet names longer than this
  cloneSheet(wb, "layout", sheet_name)
  writeWorksheet( # add the industry name to top
    wb,
    long_name,
    sheet_name,
    startRow = 1,
    startCol = 2,
    header = FALSE,
    rownames = FALSE
  )
  writeWorksheet( # add the maximum date in the data
    wb,
    current,
    sheet_name,
    startRow = 2,
    startCol = 2,
    header = FALSE,
    rownames = FALSE
  )
  writeWorksheet( # add when the sheet was written
    wb,
    now(),
    sheet_name,
    startRow = 3,
    startCol = 2,
    header = FALSE,
    rownames = FALSE
  )
  writeWorksheet( # add the data
    wb,
    tbbl,
    sheet_name,
    startRow = 5,
    startCol = 1,
    header = TRUE,
    rownames = FALSE
  )
}
stl_smooth <- function(tbbl){
  tbbl%>%
    mutate(date=yearmonth(date))%>%
    tsibble(key=name, index = date)%>%
    model(stl = STL(value~ trend(window = 5)+ season(window = "periodic"),
                    robust = TRUE))%>%
    components()%>%
    as_tsibble()%>%
    mutate(date=lubridate::ym(date))%>%
    select(name, date, smoothed=trend)%>%
    rename(value=smoothed)%>%
    as_tibble()
}

rescale01 <- function(tbbl, var, ...) {
  tbbl%>%
    mutate(value= ({{  var  }} - min({{  var  }}, ...)) / (max({{  var  }}, ...) - min({{  var  }}, ...)))
}
