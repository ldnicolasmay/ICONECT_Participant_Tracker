#!/usr/bin/env Rscript

# get_data_csv.R

#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@                                                                       @###
###                        GET, PROCESS, SAVE DATA                          ###
###                   Data Downloaded from REDCap as CSV                    ###
###@                                                                       @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#


# LOAD USEFUL LIBRARIES ----

suppressMessages( library(dplyr) )
suppressMessages( library(tidyr) )
suppressMessages( library(readr) )
suppressMessages( library(rlang) )
suppressMessages( library(purrr) )
suppressMessages( library(crayon) )
suppressMessages( library(stringr) )


# LOAD USEFUL GLOBALS / FUNCTIONS ----

source("./get_data_csv_helpers.R")


# GET RAW DATA (FROM CSV) ----

# Read raw study data from freshest CSV
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

# Throw a colorful warning if the latest data CSV wasn't downloaded today
redd <- make_style("#FF0000")
orng <- make_style("#FF7700")
yllw <- make_style("#FFFF00")
grnn <- make_style("#00DD00")
bluu <- make_style("#2B2BFF")
slvr <- silver

warning_bar <- "|------------========= WARNING =========------------|\n"
warning_msg <- "| The most recent data CSV was not downloaded today |\n"
warning_msg_length <- nchar(warning_msg)
if (as.Date(df_icdd_csvs_latest[["dl_time"]]) != Sys.Date()) {
  warning(
    bold(
      paste0("\n",
             strrep(" ", (80 - warning_msg_length) %/% 2),
             redd(warning_bar),
             strrep(" ", (80 - warning_msg_length) %/% 2),
             orng(warning_bar),
             strrep(" ", (80 - warning_msg_length) %/% 2),
             slvr(warning_msg),
             strrep(" ", (80 - warning_msg_length) %/% 2),
             yllw(warning_msg),
             strrep(" ", (80 - warning_msg_length) %/% 2),
             slvr(warning_msg),
             strrep(" ", (80 - warning_msg_length) %/% 2),
             grnn(warning_bar),
             strrep(" ", (80 - warning_msg_length) %/% 2),
             bluu(warning_bar))))
}

df <- read_csv(df_icdd_csvs_latest[[1, "filepath"]],
               col_types = cols(.default = col_character()))

# Read proxy fields data from hand-built XLSX
proxy_fields_df <- readxl::read_excel("./proxy_fields.xlsx")

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


# PROCESS DATA ----

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

# Get unique IDs
ids <-
  df_cln %>%
  distinct(ts_sub_id) %>%
  pull() %>%
  sort()

# Get redcap_event_name values as strings in a character vector
ren_strs <-
  proxy_fields_df %>%
  distinct(REN) %>%
  pull() %>%
  # for now, filter out weekly telephone calls and daily video chats
  str_remove("^w\\d{2}(d\\d)?_(tel|vc)_arm_1$") %>%
  stringi::stri_remove_empty()

# For each redcap_event_name, create a df from `df_cln` that contains only
# the fields that are identified as relevant proxy fields from the
# `proxy_fields_df`, then pack all the dfs into a list.
dfs_rens <-
  map(.x = ren_strs,
      .f = reduce_df_by_proxy_field_ren, # sourced above
      df_cln,
      proxy_fields_df)

# Use the redcap_event_name strings to name the dfs in `dfs_rens`
names(dfs_rens) <- ren_strs

# The dfs in `dfs_rens` have values scattered across several rows, so these
# need to be collapsed into one row for each participant. That's what
# `collapse_df_rows_by_ren` does.
dfs_rens_rdc <-
  map(.x = ren_strs,
      .f = collapse_df_rows_by_ren,
      dfs_rens,
      ids)

# Again, use the redcap_event_name strings to name the dfs
# in `dfs_rens_rdc`
names(dfs_rens_rdc) <- ren_strs

# Ensure that each df in dfs_rens_rdc has all possible IDs
dfs_rens_rdc_aug1 <-
  map(dfs_rens_rdc,
      function(df) {
        if ("ts_sub_id" %in% names(df)) {
          left_join(tibble(ts_sub_id = ids),
                    df,
                    by = "ts_sub_id")
        } else {
          left_join(tibble(ts_sub_id = ids),
                    bind_rows(tibble(ts_sub_id = ids),
                              df),
                    by = "ts_sub_id")
        }
      })

# Ensure that each df in dfs_rens_rdc_aug2 has all appropriate fields
dfs_rens_rdc_aug2 <-
  map2(.x = dfs_rens_rdc_aug1,
       .y = ren_strs,
       .f = add_missing_fields,
       proxy_fields_df)

# Nest all the proxy fields for each ID in order to ease identification of
# incomplete instruments/forms
dfs_rens_rdc_aug_nst <-
  map(.x = dfs_rens_rdc_aug2,
      .f = function(df) {
        df %>%
          group_by(ts_sub_id) %>%
          nest()
      })

# Add columns that help identify which instruments/forms are missing
# for every participant
dfs_rens_rdc_aug_nst_mfs <-
  map2(.x = dfs_rens_rdc_aug_nst,
       .y = ren_strs,
       .f = add_missing_forms_col,
       proxy_fields_df)


# SAVE DATA AS RDS ----

saveRDS(dfs_rens_rdc_aug_nst_mfs,
        "./ICONECT_Participant_Tracker/rds/dfs_rens_rdc_aug_nst_mfs.Rds")


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
