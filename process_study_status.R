# process_study_status.R

library(data.table)
library(dplyr)
library(stringr)

# Load all data
data <- data.table::fread("OCTRI5793Internetbas_DATA_2019-01-26_0908.csv",
                          na.strings = "")
print(object.size(data), units = "auto")
# unique(purrr::map_chr(data, class))

# Select only date (`_dat`) and form-complete (`_complete`) fields
data_slct <- data %>% 
  as_tibble(data) %>% 
  filter(is.na(redcap_repeat_instrument)) %>% # elim.s previsit stability scrn.
  select(ts_sub_id, 
         redcap_event_name, 
         ts_lfn, ts_pfn, ts_lln,
         ends_with("_dat"), ends_with("_dtc"),
         ends_with("_complete"))
print(object.size(data_slct), units = "auto")
data_slct %>% 
  distinct(ts_sub_id) %>% 
  pull()

# Get UM site IDs only
data_slct_fltr <- data_slct %>% 
  filter(str_detect(ts_sub_id, pattern = "^C2\\d{3}$"))
print(object.size(data_slct_fltr), units = "auto")
data_slct_fltr %>% 
  distinct(ts_sub_id) %>% 
  pull()

# Check class of each field
purrr::map(data_slct_fltr, class)
purrr::map(data_slct_fltr, unique)

# Coerce fields to appropriate type
data_slct_fltr_1 <- data_slct_fltr %>% 
  select(ts_sub_id, redcap_event_name, ts_lfn, ts_pfn, ts_lln)
data_slct_fltr_2 <- data_slct_fltr %>% 
  select(ends_with("_dat"), ends_with("_dtc"))
data_slct_fltr_3 <- data_slct_fltr %>% 
  select(ends_with("_complete"))

data_slct_fltr_2 <- purrr::map_df(data_slct_fltr_2, as.Date)
data_slct_fltr_3 <- purrr::map_df(data_slct_fltr_3, as.integer)

data_slct_fltr <- bind_cols(data_slct_fltr_1,
                            data_slct_fltr_2,
                            data_slct_fltr_3)
rm(data_slct_fltr_1); rm(data_slct_fltr_2); rm(data_slct_fltr_3);
print(object.size(data_slct_fltr), units = "auto")

uniq_ids <- data_slct_fltr %>% 
  distinct(ts_sub_id) %>% 
  pull() %>% 
  sort()

fwrite(data_slct_fltr, "data_slct_fltr.csv", na = "")

####################################

stages_not_is_na <- 
  list(scrn_tel_arm_1 = c("ts_dat"), 
       scrn_v_arm_1   = c("con_dtc",
                          "em_dat",
                          "dem_dat",
                          "vis_dat",
                          "hr_dat",
                          "moc_dat",
                          "lsn_dat",
                          "a5_dat",
                          "rx_dat",
                          "b6_dat",
                          "dsa_dat",
                          "elg_dat",
                          "mrp_dat"),
       bv1_arm_1      = c("cdr_dat",
                          "phy_dat",
                          "c2_dat",
                          "neo_dat"),
       bl_cdx_arm_1   = c("d1_dat"))

stages_eq_two <- 
  list(scrn_tel_arm_1 = c("telephone_screening_complete"),
       scrn_v_arm_1   = c("consent_admin_form_complete",
                          "emergency_contact_complete",
                          "nacc_a1_demographics_complete",
                          "vision_test_complete",
                          "hearing_test_complete",
                          "nacc_moca_complete",
                          "lubben_social_network_scale_complete",
                          "nacc_a5_health_history_complete",
                          "rx_norm_medication_form_complete",
                          "nacc_b6_depression_scale_complete",
                          "depression_safety_assessment_complete",
                          "baseline_eligibility_form_complete",
                          "mri_preliminary_screening_complete"),
       bv1_arm_1      = c("nacc_b4_cdr_complete",
                          "nacc_b1_physical_evaluation_complete",
                          "nacc_c2_neuropsych_scores_complete",
                          "neoffi_personality_inventory_complete"),
       bl_cdx_arm_1   = c("nacc_d1_clinician_diagnosis_complete"))


# Checks if all date fields from redcap_event_name row for a given pt. ID
# are not NA in the supplied dataframe
date_fields_complete <- function(df, uniq_id, redcap_event_name) {
  categ_date =
      df[df[["ts_sub_id"]] == uniq_id &
                       df[["redcap_event_name"]] == redcap_event_name,
                     stages_not_is_na[[redcap_event_name]]]
  categ_date = unlist(categ_date)
  ifelse(length(categ_date) > 0,
         all(!is.na(categ_date)),
         FALSE)
}

# Checks if all form_compete fields fields from redcap_event_name row 
# for a given pt. ID are complete in the supplied dataframe
form_fields_complete <- function(df, uniq_id, redcap_event_name) {
  categ_comp =
      df[df[["ts_sub_id"]] == uniq_id &
           df[["redcap_event_name"]] == redcap_event_name,
         stages_eq_two[[redcap_event_name]]]
  categ_comp = unlist(categ_comp)
  ifelse(length(categ_comp) > 0,
         all(categ_comp == 2),
         FALSE)
}

for (uniq_id in uniq_ids) {
  # name
  fname <- 
    unlist(
      data_slct_fltr[data_slct_fltr$ts_sub_id == uniq_id & 
                       data_slct_fltr$redcap_event_name == "scrn_tel_arm_1", 
                     "ts_lfn"]
    )
  lname <- 
    unlist(
      data_slct_fltr[data_slct_fltr$ts_sub_id == uniq_id &
                       data_slct_fltr$redcap_event_name == "scrn_tel_arm_1",
                     "ts_lln"]
    )
  
  # telephone screening complete?
  scrn_tel_date <- 
    date_fields_complete(data_slct_fltr, uniq_id, "scrn_tel_arm_1")
  scrn_tel_comp <-
    form_fields_complete(data_slct_fltr, uniq_id, "scrn_tel_arm_1")

  # screening visit complete?
  scrn_v_date <-
    date_fields_complete(data_slct_fltr, uniq_id, "scrn_v_arm_1")
  scrn_v_comp <-
    form_fields_complete(data_slct_fltr, uniq_id, "scrn_v_arm_1")
  
  # baseline visit 1 complete?
  bv1_date <-
    date_fields_complete(data_slct_fltr, uniq_id, "bv1_arm_1")
  bv1_comp <-
    form_fields_complete(data_slct_fltr, uniq_id, "bv1_arm_1")
  
  # basline diagnosis complete?
  bl_cdx_date <-
    date_fields_complete(data_slct_fltr, uniq_id, "bl_cdx_arm_1")
  bl_cdx_comp <-
    form_fields_complete(data_slct_fltr, uniq_id, "bl_cdx_arm_1")
  
  # cat out
  cat(paste(uniq_id, 
            "\n name:", fname, lname, 
            "\n scrn_tel:", scrn_tel_date, scrn_tel_comp, 
            "\n scrn_v:", scrn_v_date, scrn_v_comp, 
            "\n bv1:", bv1_date, bv1_comp,
            "\n bl_cdx", bl_cdx_date, bl_cdx_comp, 
            "\n"))
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
