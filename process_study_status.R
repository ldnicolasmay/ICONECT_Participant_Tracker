#!/usr/bin/env Rscript

# process_study_status.R

library(dplyr)
library(readr)
library(rlang)
library(crayon)
library(stringr)

# GET USEFUL HELPERS / GLOBALS ----
source("./process_study_status_helpers.R")

# GET DATA ----

# Read raw study data from CSV
# df <- readr::read_csv("./OCTRI5793Internetbas_DATA_2019-02-28_1145.csv")
df_icdd_csvs <- 
  file.info(paste0("./data_dump/", list.files("./data_dump/"))) %>% 
  as_tibble(rownames = "filepath") %>% 
  filter(!isdir) %>% 
  select(filepath) %>% 
  mutate(dl_time = 
           str_extract(filepath, "\\d{4}-\\d{2}-\\d{2}_\\d{4}") %>% 
           str_replace("(_)(\\d{2})(\\d{2})", " \\2:\\3") %>% 
           str_c(":00") %>% 
           lubridate::as_datetime()
  )

df_icdd_csvs_latest <-
  df_icdd_csvs %>% 
  arrange(desc(dl_time)) %>% 
  slice(1L)

redd <- make_style("#FF0000")
orng <- make_style("#FF7700")
yllw <- make_style("#FFFF00")
grnn <- make_style("#00DD00")
bluu <- make_style("#2B2BFF")
slvr <- silver

# Throw a warning if the latest data CSV wasn't downloaded today
if (df_icdd_csvs_latest[["dl_time"]] != Sys.Date()) {
  warning(
    bold(
      paste0("\n",
             strrep(" ", 14),
             redd("+------------========= WARNING =========------------+\n"),
             strrep(" ", 14),
             orng("|------------                           ------------|\n"),
             strrep(" ", 14),
             slvr("| The most recent data CSV was not downloaded today |\n"),
             strrep(" ", 14),
             yllw("| The most recent data CSV was not downloaded today |\n"),
             strrep(" ", 14),
             slvr("| The most recent data CSV was not downloaded today |\n"),
             strrep(" ", 14),
             grnn("|------------                           ------------|\n"),
             strrep(" ", 14),
             bluu("+------------========= WARNING =========------------+\n"))))
}

df <- read_csv(df_icdd_csvs_latest[[1, "filepath"]],
               col_types = cols(.default = col_character()))

# Read proxy fields data from XLSX
proxy_fields_df <- readxl::read_excel("./proxy_fields.xlsx")

# PROCESS DATA ----

# Define study data fields to keep
keeper_fields <-
  c(
    "ts_sub_id"
    , "redcap_event_name"
    , "redcap_repeat_instrument"
    , "redcap_repeat_instance"
    , "ps_stt"
    , unique(proxy_fields_df$Field)
  )

# Define redcap_event_name values to keep
keeper_RENs <- 
  c(
    "admin_arm_1"
    , unique(proxy_fields_df$REN)
  )

# _ Clean Data ----

# Get only fields and rows of interest
df_cln <- df %>% 
  select(keeper_fields) %>%
  filter(str_detect(ts_sub_id, "^C\\d{4}$")) %>% 
  filter(redcap_event_name %in% keeper_RENs) %>% 
  type_convert(col_types = cols(
    .default = col_date(),
    ts_sub_id = col_character(),
    redcap_event_name = col_character(),
    redcap_repeat_instrument = col_character(),
    redcap_repeat_instance = col_character(),
    ps_stt = col_integer()
  ))

# _ Process Data ----

ids <-
  df_cln %>% 
  distinct(ts_sub_id) %>% 
  pull() %>% 
  sort()

ren_strs <- 
  proxy_fields_df %>% 
  distinct(REN) %>% 
  pull() %>% 
  # for now, filter out weekly telephone calls and daily video chats
  str_remove("^w\\d{2}(d\\d)?_(tel|vc)_arm_1$") %>% 
  stringi::stri_remove_empty()
# print(ren_strs)

dfs_rens <-
  purrr::map(.x = ren_strs, 
             .f = reduce_df_by_proxy_field_ren, # sourced above
             proxy_fields_df, df_cln)

names(dfs_rens) <- ren_strs

# # scrn_tel_arm_1
# glimpse(dfs_rens[["scrn_tel_arm_1"]])
# dfs_rens[["scrn_tel_arm_1"]] %>% 
#   mutate_all(as.character) %>% 
#   purrr::map_dfr(.x = ids,
#                  .f = merge_df_rows_by_id_and_cols,
#                  ts_sub_id, 
#                  ., 
#                  proxy_fields_df %>% 
#                    filter(REN == "scrn_tel_arm_1") %>% 
#                    pull(Field))
# 
# # scrn_v_arm_1
# glimpse(dfs_rens[["scrn_v_arm_1"]])
# dfs_rens[["scrn_v_arm_1"]] %>% 
#   mutate_all(as.character) %>% 
#   purrr::map_dfr(.x = ids,
#                  .f = merge_df_rows_by_id_and_cols,
#                  ts_sub_id, 
#                  ., 
#                  proxy_fields_df %>% 
#                    filter(REN == "scrn_v_arm_1") %>% 
#                    pull(Field))
# 
# # bl_cdx_arm_1
# glimpse(dfs_rens[["bl_cdx_arm_1"]])
# dfs_rens[["bl_cdx_arm_1"]] %>% 
#   mutate_all(as.character) %>% 
#   purrr::map_dfr(.x = ids,
#                  .f = merge_df_rows_by_id_and_cols,
#                  ts_sub_id, 
#                  ., 
#                  proxy_fields_df %>% 
#                    filter(REN == "bl_cdx_arm_1") %>% 
#                    pull(Field))
# 
# # bl_mri_arm_1
# glimpse(dfs_rens[["bl_mri_arm_1"]])
# dfs_rens[["bl_mri_arm_1"]] %>% 
#   mutate_all(as.character) %>% 
#   purrr::map_dfr(.x = ids,
#                  .f = merge_df_rows_by_id_and_cols,
#                  ts_sub_id, 
#                  ., 
#                  proxy_fields_df %>% 
#                    filter(REN == "bl_mri_arm_1") %>% 
#                    pull(Field))
# 
# # bl_v_arm_1
# glimpse(dfs_rens[["bl_v_arm_1"]])
# dfs_rens[["bl_v_arm_1"]] %>% 
#   mutate_all(as.character) %>% 
#   purrr::map_dfr(.x = ids,
#                  .f = merge_df_rows_by_id_and_cols,
#                  ts_sub_id, 
#                  ., 
#                  proxy_fields_df %>% 
#                    filter(REN == "bl_v_arm_1") %>% 
#                    pull(Field))
# 
# # 06_cdx_arm_1
# glimpse(dfs_rens[["06_cdx_arm_1"]])
# dfs_rens[["06_cdx_arm_1"]] %>% 
#   mutate_all(as.character) %>% 
#   purrr::map_dfr(.x = ids,
#                  .f = merge_df_rows_by_id_and_cols,
#                  ts_sub_id, 
#                  ., 
#                  proxy_fields_df %>% 
#                    filter(REN == "06_cdx_arm_1") %>% 
#                    pull(Field))
# 
# # 06_mri_arm_1
# glimpse(dfs_rens[["06_mri_arm_1"]])
# dfs_rens[["06_mri_arm_1"]] %>% 
#   mutate_all(as.character) %>% 
#   purrr::map_dfr(.x = ids,
#                  .f = merge_df_rows_by_id_and_cols,
#                  ts_sub_id, 
#                  ., 
#                  proxy_fields_df %>% 
#                    filter(REN == "06_mri_arm_1") %>% 
#                    pull(Field))
# 
# # 06_mri_arm_1
# glimpse(dfs_rens[["06_v_arm_1"]])
# dfs_rens[["06_v_arm_1"]] %>% 
#   mutate_all(as.character) %>% 
#   purrr::map_dfr(.x = ids,
#                  .f = merge_df_rows_by_id_and_cols,
#                  ts_sub_id, 
#                  ., 
#                  proxy_fields_df %>% 
#                    filter(REN == "06_v_arm_1") %>% 
#                    pull(Field))
# 
# # 12_cdx_arm_1
# glimpse(dfs_rens[["12_cdx_arm_1"]])
# dfs_rens[["12_cdx_arm_1"]] %>% 
#   mutate_all(as.character) %>% 
#   purrr::map_dfr(.x = ids,
#                  .f = merge_df_rows_by_id_and_cols,
#                  ts_sub_id, 
#                  ., 
#                  proxy_fields_df %>% 
#                    filter(REN == "12_cdx_arm_1") %>% 
#                    pull(Field))
# 
# # 12_v_arm_1
# glimpse(dfs_rens[["12_v_arm_1"]])
# dfs_rens[["12_v_arm_1"]] %>% 
#   mutate_all(as.character) %>% 
#   purrr::map_dfr(.x = ids,
#                  .f = merge_df_rows_by_id_and_cols,
#                  ts_sub_id, 
#                  ., 
#                  proxy_fields_df %>% 
#                    filter(REN == "12_v_arm_1") %>% 
#                    pull(Field))
# 
# # fup_tel_arm_1
# glimpse(dfs_rens[["fup_tel_arm_1"]])
# dfs_rens[["fup_tel_arm_1"]] %>% 
#   mutate_all(as.character) %>% 
#   purrr::map_dfr(.x = ids,
#                  .f = merge_df_rows_by_id_and_cols,
#                  ts_sub_id, 
#                  ., 
#                  proxy_fields_df %>% 
#                    filter(REN == "fup_tel_arm_1") %>% 
#                    pull(Field))
# 
# collapse_df_rows_by_ren("scrn_tel_arm_1", dfs_rens)
# collapse_df_rows_by_ren("scrn_v_arm_1", dfs_rens)

dfs_rens_collapsed <-
  purrr::map(.x = ren_strs,
             .f = collapse_df_rows_by_ren,
             dfs_rens)

names(dfs_rens_collapsed) <- ren_strs


###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
