# process_study_status.R

library(dplyr)
library(stringr)
library(readr)

# GET DATA ----

# Read raw study data from CSV
df <- readr::read_csv("./OCTRI5793Internetbas_DATA_2019-02-28_1145.csv")

# Read proxy fields data from XLSX
proxy_fields <- readxl::read_excel("./proxy_fields.xlsx")

# PROCESS DATA ----

# Define study data fields to keep
keeper_fields <-
  c(
    "ts_sub_id"
    , "redcap_event_name"
    , "redcap_repeat_instrument"
    , "redcap_repeat_instance"
    , "ps_stt"
    , unique(proxy_fields$Field)
  )

# Define redcap_event_name values to filter for
keeper_RENs <- 
  c(
    "admin_arm_1"
    , unique(proxy_fields$REN)
  )
  
# _ Clean Data ----

# Get only fields and rows of interest
df_cln <- df %>% 
  select(keeper_fields) %>% 
  filter(redcap_event_name %in% keeper_RENs) %>% 
  type_convert(col_types = cols(
    .default = col_date(),
    ts_sub_id = col_character(),
    redcap_event_name = col_character(),
    redcap_repeat_instrument = col_character(),
    redcap_repeat_instance = col_character(),
    ps_stt = col_integer()
  ))













