# get_data_helpers.R

#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@                                                                       @###
###                 HELPER FUNCTIONS FOR `get_data_api.R`                   ###
###@                                                                       @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#


#' `reduce_df_by_proxy_field_ren`
#'
#' Given a redcap_event_name string, `proxies_df`, and a dataframe,
#' extract the rows and columns that correspond with those associated to
#' the redcap_event_name string in `proxies_df`
#'
reduce_df_by_proxy_field_ren <- function(ren_str, df, proxies_df) {
  # print(ren_str)
  df %>%
    filter(redcap_event_name %in%
             pull(filter(proxies_df, REN == ren_str), REN)) %>%
    select(ts_sub_id
           , redcap_event_name
           , redcap_repeat_instrument
           , redcap_repeat_instance,
           !!!syms(pull(filter(proxies_df, REN == ren_str), Field)))
}

#' `collapse_df_rows_by_ren`
#'
#' For each df in a dfs list , collapse data scattered across different rows of
#' the df into one row by ID
#'
#' Given:
#' dfs[[ ren_str[1] ]] =
#'   | ID | field_1 | field_2 | field_3 |
#'   |----+---------+---------+---------|
#'   | 1  | a       |         |         |
#'   | 2  | a       |         |         |
#'   | 2  |         | b       |         |
#'   | 3  | a       |         |         |
#'   | 3  |         | b       |         |
#'   | 3  |         |         | c       |
#'
#' Return:
#' df[[ ren_str[1] ]] =
#'   | ID | field_1 | field_2 | field_3 |
#'   |----+---------+---------+---------|
#'   | 1  | a       |         |         |
#'   | 2  | a       | b       |         |
#'   | 3  | a       | b       | c       |
#'
collapse_df_rows_by_ren <- function(ren_str, dfs, ids) {

  dfs[[ren_str]] %>%
    mutate_all(as.character) %>%
    purrr::map_dfr(.x = ids,
                   .f = merge_df_rows_by_id_and_cols,
                   ts_sub_id,
                   .,
                   proxy_fields_df %>%
                     filter(REN == ren_str) %>%
                     pull(Field))

}

#' `merge_df_rows_by_id_and_cols`
#'
#' Helper function for `collapse_df_rows_by_ren`
#'
#' Given a pt. ID, the field name containing the IDs, a df, and the columns
#' to keep, merge all the rows down to one row
#'
merge_df_rows_by_id_and_cols <- function(id, id_col, df, cols_vct) {

  cols <- parse_exprs(cols_vct)

  df_iso <- df %>%
    select(!!enquo(id_col), !!!cols) %>%
    isolate_df_rows_by_id(!!enquo(id_col), id)

  if (nrow(df_iso) == 0L) {
    return(NULL)
  } else if (nrow(df_iso) == 1L) {
    return(df_iso)
  }

  df_ret <- df_iso %>% slice(1L)

  for (i in 2:nrow(df_iso)) {
    v1 <- df_ret %>% select(!!!cols) %>% slice(1L) %>% as.character()
    v2 <- df_iso %>% select(!!!cols) %>% slice(i) %>% as.character()
    v <- zip_vectors(v1, v2)

    df_ret[1, as.character(cols_vct)] <- v
  }

  df_ret
}

#' `isolate_df_rows_by_id`
#'
#' Helper function for `merge_df_rows_by_id_and_cols`
#'
#' Given a df, the field name of the ID column, and an ID, extract only
#' those rows with the given ID
#'
isolate_df_rows_by_id <- function(df, id_col, id) {
  df %>%
    filter(!!enquo(id_col) == id)
}

#' `zip_vectors`
#'
#' Helper function for `merge_df_rows_by_id_and_cols`
#'
#' Given two vectors like `c(1, NA, NA)` and `c(NA, 2, 3)`, return `c(1, 2, 3)`
#'
zip_vectors <- function(v1, v2) {
  v <- v1
  v[is.na(v1)] <- v2[is.na(v1)]
  v[is.na(v2)] <- v1[is.na(v2)]
  v
}

#' `add_missing_fields`
#'
#' Given a df, redcap_event_name string, and `proxy_fields_df`,
#' ensure that the df has all appropriate fields based on those that
#' correspond with the redcap_event_name string in `proxy_fields_df`
#'
add_missing_fields <- function(df, ren, proxy_fields_df) {

  cols_to_exclude <- names(df)

  cols_to_add <- proxy_fields_df %>%
    filter(REN == ren) %>%
    pull(Field)

  cols_to_add <- cols_to_add[!(cols_to_add %in% cols_to_exclude)]

  if (!identical(cols_to_add, character(0))) {
    df[, cols_to_add] <- NA
    return(df)
  } else {
    return(df)
  }

}

#' `add_complete_col`
#'
#' Given a df, redcap_event_name string, and `proxy_fields_df`,
#' add a column that shows whether a given stage is complete
#' for each participant
#'
add_complete_col <- function(df, ren, proxy_fields_df) {

  # select_proxy_fields_df = proxy_fields_df %>%
  #   select(Field, Form, REN) %>%
  #   filter(REN == ren)
  #
  # select_proxy_fields_flds = select_proxy_fields_df %>%
  #   pull(Field)

  df %>%
    rowwise() %>%
    mutate(complete = case_when(
      sum(is.na(data)) == 0 ~ "Yes",
      sum(!is.na(data)) == 0 ~ "No",
      TRUE ~ "Pending"
    )) %>%
    ungroup()
}

#' `add_status_col`
#'
#' 1 In Screening
#' 2 Screen Failure
#' 3 Enrolled
#' 4 Active
#' 5 In Follow-Up
#' 6 Discontinued
#'
add_status_col <- function(df, ren, proxy_fields_df) {
  if ("ps_stt" %in% names(df[[1, "data"]])) {
  df %>%
    rowwise() %>%
    mutate(complete = case_when(
      data[[1, "ps_stt"]] == 1 ~ "In Screening",
      data[[1, "ps_stt"]] == 2 ~ "Screen Failure",
      data[[1, "ps_stt"]] == 3 ~ "Enrolled",
      data[[1, "ps_stt"]] == 4 ~ "Active",
      data[[1, "ps_stt"]] == 5 ~ "In Follow-Up",
      data[[1, "ps_stt"]] == 6 ~ "Discontinued",
      TRUE ~ NA_character_
    )) %>%
    ungroup()
  }
}

#' `add_missing_forms_col`
#'
#' Given a df, redcap_even_name string, and `proxy_fields_df`,
#' add columns that identify which forms are missing
#' for each participant
#'
add_missing_forms_col <- function(df, ren, proxy_fields_df) {

  select_proxy_fields_df = proxy_fields_df %>%
    select(Field, Form, REN) %>%
    filter(REN == ren)

  select_proxy_fields_flds = select_proxy_fields_df %>%
    pull(Field)

  df %>%
    rowwise() %>%
    mutate(
      missing_dates =
        list(
          names(data)[is.na(data[1, select_proxy_fields_flds])]
        )
    ) %>%
    mutate(
      missing_forms_list = proxy_fields_df %>%
        filter(REN == ren) %>%
        filter(Field %in% missing_dates) %>%
        pull(Form) %>%
        list()
    ) %>%
    mutate(
      missing_forms = paste(unlist(missing_forms_list), collapse = "<br/> ")
    ) %>%
    ungroup()

}


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
