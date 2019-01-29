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
         ends_with("_dat"), ends_with("_dtc"), ends_with("date"),
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
  select(ends_with("_dat"), ends_with("_dtc"), ends_with("date"))
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
       bl_cdx_arm_1   = c("d1_dat"),
       bv2_arm_1      = c("date",
                          "otd_dat",
                          "fhd_dat",
                          "ap_dat"),
       admin_arm_1    = character(0), # No dates for video chat randomization
       bl_mri_arm_1   = c("mrs_dat")
  )

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
       bl_cdx_arm_1   = c("nacc_d1_clinician_diagnosis_complete"),
       bv2_arm_1      = c("nih_toolbox_complete",
                          "otdlr_administration_complete",
                          "otdlr_composite_scores_complete",
                          "family_history_of_dementia_complete",
                          "apoe_complete"),
       admin_arm_1    = c("video_chat_randomization_form_complete"),
       bl_mri_arm_1   = c("mri_scheduling_form_complete")
  )

# Get a specific value from the supplied dataframe
get_value <- function(uniq_id, redcap_event_name, field, df) {
  unlist(
    df[df$ts_sub_id == uniq_id &
         df$redcap_event_name == redcap_event_name,
       field]
  )
}

# Checks if all date fields from redcap_event_name row for a given pt. ID
# are not NA in the supplied dataframe
date_fields_complete <- function(uniq_id, redcap_event_name, df) {
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
form_fields_complete <- function(uniq_id, redcap_event_name, df) {
  categ_comp =
    df[df[["ts_sub_id"]] == uniq_id &
         df[["redcap_event_name"]] == redcap_event_name,
       stages_eq_two[[redcap_event_name]]]
  categ_comp = unlist(categ_comp)
  ifelse(length(categ_comp) > 0,
         all(categ_comp == 2),
         FALSE)
}

build_distilled_row <- function(uniq_id, df) {

  # ID
  ts_sub_id = uniq_id
    
  # Pt. name
  ts_lfn = get_value(uniq_id, "scrn_tel_arm_1", "ts_lfn", data_slct_fltr)
  ts_pfn = get_value(uniq_id, "scrn_tel_arm_1", "ts_pfn", data_slct_fltr)
  ts_lln = get_value(uniq_id, "scrn_tel_arm_1", "ts_lln", data_slct_fltr)
  
  # telephone screening complete?
  scrn_tel_date =
    date_fields_complete(uniq_id, "scrn_tel_arm_1", data_slct_fltr)
  scrn_tel_comp =
    form_fields_complete(uniq_id, "scrn_tel_arm_1", data_slct_fltr)
  
  # screening visit complete?
  scrn_v_date =
    date_fields_complete(uniq_id, "scrn_v_arm_1", data_slct_fltr)
  scrn_v_comp =
    form_fields_complete(uniq_id, "scrn_v_arm_1", data_slct_fltr)
  
  # baseline visit 1 complete?
  bv1_date =
    date_fields_complete(uniq_id, "bv1_arm_1", data_slct_fltr)
  bv1_comp =
    form_fields_complete(uniq_id, "bv1_arm_1", data_slct_fltr)
  
  # basline diagnosis complete?
  bl_cdx_date =
    date_fields_complete(uniq_id, "bl_cdx_arm_1", data_slct_fltr)
  bl_cdx_comp =
    form_fields_complete(uniq_id, "bl_cdx_arm_1", data_slct_fltr)
  
  # baseline visit 2 complete?
  bv2_date =
    date_fields_complete(uniq_id, "bv2_arm_1", data_slct_fltr)
  bv2_comp =
    form_fields_complete(uniq_id, "bv2_arm_1", data_slct_fltr)
  
  # video chat randomization complete?
  admin_date = NA
  admin_comp =
    form_fields_complete(uniq_id, "admin_arm_1", data_slct_fltr)
  
  # baseline mri scheduling complete?
  bl_mri_date =
    date_fields_complete(uniq_id, "bl_mri_arm_1", data_slct_fltr)
  bl_mri_comp =
    form_fields_complete(uniq_id, "bl_mri_arm_1", data_slct_fltr)
  
  tibble(ts_sub_id,
         ts_lfn, ts_pfn, ts_lln,
         scrn_tel_date, scrn_tel_comp,
         scrn_v_date,   scrn_v_comp,
         bv1_date,      bv1_comp,
         bl_cdx_date,   bl_cdx_comp,
         bv2_date,      bv2_comp,
         admin_date,    admin_comp,
         bl_mri_date,   bl_mri_comp
  )
}

# row_c2001 <- build_distilled_row("C2001", data_slct_fltr)
# row_c2002 <- build_distilled_row("C2002", data_slct_fltr)
# 
# class(row_c2001)
# 
# rbind(row_c2001, row_c2002)
# bind_rows(row_c2001, row_c2002)
# blah <- bind_rows(row_c2001, row_c2002)
# View(blah)

distilled_df <- purrr::map_df(uniq_ids, build_distilled_row, data_slct_fltr)




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
