##Validation
library(js)
library(xlsx)
library(openxlsx)
library(dplyr)
library(janitor)
library(data.table)
library(tidyr)
library(lubridate)
library(ggplot2)
library(quarto)
library(stringr)
library(purrr)

#reference http://uc-r.github.io/scraping#importing_spreadsheet_data
path <- "FY26_detentionStats05032025.xlsx"
# use read.xlsx to import 
df <- openxlsx::read.xlsx(path, sheet = 7)
fy26_fac_locations <- read.csv("fy26_detention_output_2025-11-22.csv")


coords <- fy26_fac_locations |>
  select(c(Name, Geocodio.Longitude, Geocodio.Latitude))|>
  mutate(Name = str_to_upper(Name))|>
  rename(Longitude = Geocodio.Longitude,
         Latitude = Geocodio.Latitude)
# identify facilities

facilities <- df |>
  row_to_names(row_number = 6) |> #Possible touchpoint here to identify rownames
  #filter(!if_any(`Last Final Rating`, is.na))|>
  filter(!is.na(`Level A`))|>
  distinct(Name, City) |>
  arrange(Name) |>
  select(Name) #|>
  #filter(Name != "CLINTON COUNTY JAIL") |>
  #filter(Name != "POLK COUNTY JAIL")|>
  #filter(Name != "TORRANCE/ESTANCIA, NM") |>
  #filter(Name != "NORTHWEST STATE CORRECTIONAL CENTER") |>
  #filter(Name != "STE. GENEVIEVE COUNTY SHERIFF/JAIL")

facilities_final <- facilities |>
  left_join(coords, by="Name")|> 
  filter(!is.na(Latitude))# |>
  #filter(row_number()>35)

look_for_these <- facilities |>
  anti_join(facilities_final)
# 
# guaynabo <- c("GUAYNABO MDC (SAN JUAN)", -66.11096680236767, 18.42355109316088)
# san_juan <- c("SAN JUAN STAGING", -66.11395615631898, 18.423532527448135)
# migrant_ops <- c("MIGRANT OPS CENTER MAIN A", -75.09755939809973, 19.909152069369565)
# camp_six <- c("JTF CAMP SIX", -75.09755939809973, 19.909152069369565)

facilities_final <- facilities_final |>
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))
#filter(row_number()>110) |>

params_list <- pmap(facilities_final, list) 


# queue reports
reports <-
  tibble(
    input = rep("ice_detention_reporting.qmd", nrow(facilities_final)),
    execute_params = params_list,
    output_file = str_glue("{facilities_final$Name}.html")
  )  

pwalk(reports, quarto_render)


#report_{facilities_final$Name}_{Sys.Date()}