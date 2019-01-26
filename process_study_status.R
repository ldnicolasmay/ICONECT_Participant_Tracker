# process_study_status.R

library(dplyr)
library(stringr)

# Load all data
data <- readr::read_csv("./OCTRI5793Internetbas_DATA_2019-01-26_0908.csv") %>% 
  na_if("")
print(object.size(data), units = "auto")

# Select only date (`_dat`) and form-complete (`_complete`) fields
data_slct <- data %>% 
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

# Check class of each field; Correct if necessary
purrr::map(data_slct_fltr, class)
purrr::map(data_slct_fltr, unique)

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
print(object.size(data_slct_fltr), units = "auto")

uniq_ids <- data_slct_fltr %>% 
  distinct(ts_sub_id) %>% 
  pull() %>% 
  sort()

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
                          "mrp_dat"))
stages_not_is_na["scrn_tel_arm_1"]
stages_not_is_na[["scrn_tel_arm_1"]]
stages_not_is_na[["scrn_tel_arm_1"]][1]
stages_not_is_na["scrn_v_arm_1"]
stages_not_is_na[["scrn_v_arm_1"]]
stages_not_is_na[["scrn_v_arm_1"]][1]

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
                          "mri_preliminary_screening_complete"))
stages_eq_two["scrn_tel_arm_1"]
stages_eq_two[["scrn_tel_arm_1"]]
stages_eq_two[["scrn_tel_arm_1"]][1]
stages_eq_two["scrn_v_arm_1"]
stages_eq_two[["scrn_v_arm_1"]]
stages_eq_two[["scrn_v_arm_1"]][1]

pull(data_slct_fltr[data_slct_fltr$ts_sub_id == "C2002" & 
                      data_slct_fltr$redcap_event_name == "scrn_tel_arm_1",
                    "ts_lfn"])

for (uniq_id in uniq_ids) {
  # first name, last name
  fname <- 
    data_slct_fltr[data_slct_fltr$ts_sub_id == uniq_id & 
                     data_slct_fltr$redcap_event_name == "scrn_tel_arm_1", 
                   "ts_lfn"] %>% pull()
  lname <- 
    data_slct_fltr[data_slct_fltr$ts_sub_id == uniq_id &
                     data_slct_fltr$redcap_event_name == "scrn_tel_arm_1",
                   "ts_lln"] %>% pull()
  
  # telephone screening complete?
  scrn_tel_dat <-
    data_slct_fltr[data_slct_fltr$ts_sub_id == uniq_id &
                     data_slct_fltr$redcap_event_name == "scrn_tel_arm_1",
                   stages_not_is_na[["scrn_tel_arm_1"]] ]
                   # "ts_dat"]
  scrn_tel_comp <-
    data_slct_fltr[data_slct_fltr$ts_sub_id == uniq_id &
                     data_slct_fltr$redcap_event_name == "scrn_tel_arm_1",
                   stages_eq_two[["scrn_tel_arm_1"]] ]
                   # "telephone_screening_complete"]
  
  scrn_v_dat <-
    data_slct_fltr[data_slct_fltr$ts_sub_id == uniq_id &
                     data_slct_fltr$redcap_event_name == "scrn_v_arm_1",
                   stages_not_is_na[["scrn_v_arm_1"]] ]
  scrn_v_dat_comp 
    
    cat(paste(uniq_id, fname, lname, 
              scrn_tel_dat, scrn_tel_comp, 
              # , 
              "\n"))
}

scrn_v_dat <-
  data_slct_fltr[data_slct_fltr$ts_sub_id == "C2002" &
                   data_slct_fltr$redcap_event_name == "scrn_v_arm_1",
                 stages_not_is_na[["scrn_v_arm_1"]]
                 ]

unlist(scrn_v_dat[1, ])
all(!is.na(unlist(scrn_v_dat[1, ])))
any(is.na(unlist(scrn_v_dat[1, ])))



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
