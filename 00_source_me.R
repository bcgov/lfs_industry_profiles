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
#'
#' Get an RTRA account. (application form in directory `SAS`)
#' upload the 2 .SAS files in directory `SAS` to https://www75.statcan.gc.ca/eft-tef/en/operations (To StatCan).
#' grab a coffee...
#' download the 2 resulting csv files (From StatCan) and place in directory "data/current".
#' then source this file.

#' Note that Jan 2026 the SAS files will need to be updated.
#'
#' Output found in directory out/current.

library(assertthat)
library(here)

if (!dir.exists("data")) {dir.create("data")}
if (!dir.exists(file.path("data","current"))) {dir.create(file.path("data","current"))}
if (!dir.exists(file.path("data","old"))) {dir.create(file.path("data","old"))}
if (!dir.exists("out")) {dir.create("out")}
if (!dir.exists(file.path("out","current"))) {dir.create(file.path("out","current"))}
if (!dir.exists(file.path("out","old"))) {dir.create(file.path("out","old"))}
if (!dir.exists("temp")) {dir.create("temp")}

#are the required files where they are supposed to be?----------------
assert_that(length(list.files(here::here("data","current"), pattern="ftptemp4digNAICS"))==2,
            msg="2 files with the pattern ftptemp4digNAICS must be in folder data/current")
assert_that(length(list.files(here::here("data","current"), pattern="lfsstat4digNAICS"))==2,
            msg="2 files with the pattern lfsstat4digNAICS must be in folder data/current")
assert_that(length(list.files(here::here("data"), pattern="mapping.xlsx"))==1,
            msg="The file mapping.xlsx must be in folder data")
assert_that(length(list.files(here::here("data"), pattern="template"))==1,
            msg="The file template.xlsx must be in folder data")

#archive old output
filesstrings::file.move(here("out","current", list.files(here::here("out", "current"))),
                        here::here("out", "old"))
#create new output
source("01_process_data.R")
rmarkdown::render("02_dashboard.Rmd",
                  output_file =  str_replace_all(paste0("LFS_industry_profiles_",date(),".html")," ","_"),
                  output_dir = here::here("out","current"))


#archive ita input files--------
#filesstrings::file.move(here("data","current_ita", list.files(here("data", "current_ita"))), here("data", "old_ita"))





