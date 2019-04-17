# process_study_status_helpers.R


reduce_df_by_proxy_field_ren <- function(ren_str, proxies_df, df) {
  df %>% 
    filter(redcap_event_name %in%
             pull(filter(proxies_df, REN == ren_str), REN)) %>% 
    select(ts_sub_id
           , redcap_event_name
           , redcap_repeat_instrument
           , redcap_repeat_instance,
           !!!syms(pull(filter(proxies_df, REN == ren_str), Field)))
}


zip_vectors <- function(v1, v2) {
  v <- v1
  v[is.na(v1)] <- v2[is.na(v1)]
  v[is.na(v2)] <- v1[is.na(v2)]
  v
}


isolate_df_rows_by_id <- function(df, id_col, id) {
  df %>% 
    filter(!!enquo(id_col) == id)
}


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


collapse_df_rows_by_ren <- function(ren_str, dfs) {
  
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

