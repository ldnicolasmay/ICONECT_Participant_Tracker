# process_study_status.R

library(data.table)
library(dplyr)
library(stringr)

# Load all data
data <- data.table::fread("OCTRI5793Internetbas_DATA_2019-01-26_0908.csv",
                          na.strings = "")
# print(object.size(data), units = "auto")
# unique(purrr::map_chr(data, class))

# Select only date (`_dat`) and form-complete (`_complete`) fields
data_slct <- data %>% 
  as_tibble(data) %>% 
  filter(is.na(redcap_repeat_instrument)) %>% # elim.s previsit stability scrn.
  select(ts_sub_id, 
         redcap_event_name, 
         # ts_lfn, ts_pfn, ts_lln,
         ends_with("_dat"), ends_with("_dtc"), ends_with("date"),
         ends_with("_complete"))
# print(object.size(data_slct), units = "auto")
# data_slct %>% 
#   distinct(ts_sub_id) %>% 
#   pull()

# Get UM site IDs only
data_slct_fltr <- data_slct %>% 
  filter(str_detect(ts_sub_id, pattern = "^C2\\d{3}$"))
# print(object.size(data_slct_fltr), units = "auto")
# data_slct_fltr %>% 
#   distinct(ts_sub_id) %>% 
#   pull()

# Check class of each field
# purrr::map(data_slct_fltr, class)
# purrr::map(data_slct_fltr, unique)

# Coerce fields to appropriate type
data_slct_fltr_1 <- data_slct_fltr %>% 
  select(ts_sub_id, redcap_event_name, ts_lfn, ts_pfn, ts_lln)
data_slct_fltr_2 <- data_slct_fltr %>% 
  select(ends_with("_dat"), ends_with("_dtc"), ends_with("date"))
data_slct_fltr_3 <- data_slct_fltr %>% 
  select(ends_with("_complete"))

data_slct_fltr_2 <- purrr::map_df(data_slct_fltr_2, as.Date)
data_slct_fltr_3 <- purrr::map_df(data_slct_fltr_3, as.integer)

data_slct_fltr_cln <- bind_cols(data_slct_fltr_1,
                                data_slct_fltr_2,
                                data_slct_fltr_3)
rm(data_slct_fltr_1); rm(data_slct_fltr_2); rm(data_slct_fltr_3);
# print(object.size(data_slct_fltr_cln), units = "auto")

uniq_ids <- data_slct_fltr_cln %>% 
  distinct(ts_sub_id) %>% 
  pull() %>% 
  sort()

# fwrite(data_slct_fltr_cln, "data_slct_fltr_cln.csv", na = "")

####################################

stages_not_is_na <- 
  list(
    # telephone screening
    scrn_tel_arm_1 = c("ts_dat"), 
    # home screening
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
    # baseline visit 1
    bv1_arm_1      = c("cdr_dat",
                       "phy_dat",
                       "c2_dat",
                       "neo_dat"),
    # baseline clinician dx
    bl_cdx_arm_1   = c("d1_dat"),
    # baseline visit 2
    bv2_arm_1      = c("date",
                       "otd_dat",
                       "fhd_dat",
                       "ap_dat"),
    # randomization
    admin_arm_1    = character(0), # No dates for video chat randomization
    # baseline MRI
    bl_mri_arm_1   = c("mrs_dat"),
    # weekly phone calls
    w01_tel_arm_1  = c("wkq_dat"),
    w02_tel_arm_1  = c("wkq_dat"),
    w03_tel_arm_1  = c("wkq_dat"),
    w04_tel_arm_1  = c("wkq_dat"),
    w05_tel_arm_1  = c("wkq_dat"),
    w06_tel_arm_1  = c("wkq_dat"),
    w07_tel_arm_1  = c("wkq_dat"),
    w08_tel_arm_1  = c("wkq_dat"),
    w09_tel_arm_1  = c("wkq_dat"),
    w10_tel_arm_1  = c("wkq_dat"),
    w11_tel_arm_1  = c("wkq_dat"),
    w12_tel_arm_1  = c("wkq_dat"),
    w13_tel_arm_1  = c("wkq_dat"),
    w14_tel_arm_1  = c("wkq_dat"),
    w15_tel_arm_1  = c("wkq_dat"),
    w16_tel_arm_1  = c("wkq_dat"),
    w17_tel_arm_1  = c("wkq_dat"),
    w18_tel_arm_1  = c("wkq_dat"),
    w19_tel_arm_1  = c("wkq_dat"),
    w20_tel_arm_1  = c("wkq_dat"),
    w21_tel_arm_1  = c("wkq_dat"),
    w22_tel_arm_1  = c("wkq_dat"),
    w23_tel_arm_1  = c("wkq_dat"),
    w24_tel_arm_1  = c("wkq_dat"),
    w25_tel_arm_1  = c("wkq_dat"),
    w26_tel_arm_1  = c("wkq_dat"),
    w27_tel_arm_1  = c("wkq_dat"),
    w28_tel_arm_1  = c("wkq_dat"),
    w29_tel_arm_1  = c("wkq_dat"),
    w30_tel_arm_1  = c("wkq_dat"),
    w31_tel_arm_1  = c("wkq_dat"),
    w32_tel_arm_1  = c("wkq_dat"),
    w33_tel_arm_1  = c("wkq_dat"),
    w34_tel_arm_1  = c("wkq_dat"),
    w35_tel_arm_1  = c("wkq_dat"),
    w36_tel_arm_1  = c("wkq_dat"),
    w37_tel_arm_1  = c("wkq_dat"),
    w38_tel_arm_1  = c("wkq_dat"),
    w39_tel_arm_1  = c("wkq_dat"),
    w40_tel_arm_1  = c("wkq_dat"),
    w41_tel_arm_1  = c("wkq_dat"),
    w42_tel_arm_1  = c("wkq_dat"),
    w43_tel_arm_1  = c("wkq_dat"),
    w44_tel_arm_1  = c("wkq_dat"),
    w45_tel_arm_1  = c("wkq_dat"),
    w46_tel_arm_1  = c("wkq_dat"),
    w47_tel_arm_1  = c("wkq_dat"),
    w48_tel_arm_1  = c("wkq_dat"),
    # daily video chats
    w01d1_vc_arm_1 = c("vcd_dat"),
    w01d2_vc_arm_1 = c("vcd_dat"),
    w01d3_vc_arm_1 = c("vcd_dat"),
    w01d4_vc_arm_1 = c("vcd_dat"),
    w02d1_vc_arm_1 = c("vcd_dat"),
    w02d2_vc_arm_1 = c("vcd_dat"),
    w02d3_vc_arm_1 = c("vcd_dat"),
    w02d4_vc_arm_1 = c("vcd_dat"),
    w03d1_vc_arm_1 = c("vcd_dat"),
    w03d2_vc_arm_1 = c("vcd_dat"),
    w03d3_vc_arm_1 = c("vcd_dat"),
    w03d4_vc_arm_1 = c("vcd_dat"),
    w04d1_vc_arm_1 = c("vcd_dat"),
    w04d2_vc_arm_1 = c("vcd_dat"),
    w04d3_vc_arm_1 = c("vcd_dat"),
    w04d4_vc_arm_1 = c("vcd_dat"),
    w05d1_vc_arm_1 = c("vcd_dat"),
    w05d2_vc_arm_1 = c("vcd_dat"),
    w05d3_vc_arm_1 = c("vcd_dat"),
    w05d4_vc_arm_1 = c("vcd_dat"),
    w06d1_vc_arm_1 = c("vcd_dat"),
    w06d2_vc_arm_1 = c("vcd_dat"),
    w06d3_vc_arm_1 = c("vcd_dat"),
    w06d4_vc_arm_1 = c("vcd_dat"),
    w07d1_vc_arm_1 = c("vcd_dat"),
    w07d2_vc_arm_1 = c("vcd_dat"),
    w07d3_vc_arm_1 = c("vcd_dat"),
    w07d4_vc_arm_1 = c("vcd_dat"),
    w08d1_vc_arm_1 = c("vcd_dat"),
    w08d2_vc_arm_1 = c("vcd_dat"),
    w08d3_vc_arm_1 = c("vcd_dat"),
    w08d4_vc_arm_1 = c("vcd_dat"),
    w09d1_vc_arm_1 = c("vcd_dat"),
    w09d2_vc_arm_1 = c("vcd_dat"),
    w09d3_vc_arm_1 = c("vcd_dat"),
    w09d4_vc_arm_1 = c("vcd_dat"),
    w10d1_vc_arm_1 = c("vcd_dat"),
    w10d2_vc_arm_1 = c("vcd_dat"),
    w10d3_vc_arm_1 = c("vcd_dat"),
    w10d4_vc_arm_1 = c("vcd_dat"),
    w11d1_vc_arm_1 = c("vcd_dat"),
    w11d2_vc_arm_1 = c("vcd_dat"),
    w11d3_vc_arm_1 = c("vcd_dat"),
    w11d4_vc_arm_1 = c("vcd_dat"),
    w12d1_vc_arm_1 = c("vcd_dat"),
    w12d2_vc_arm_1 = c("vcd_dat"),
    w12d3_vc_arm_1 = c("vcd_dat"),
    w12d4_vc_arm_1 = c("vcd_dat"),
    w13d1_vc_arm_1 = c("vcd_dat"),
    w13d2_vc_arm_1 = c("vcd_dat"),
    w13d3_vc_arm_1 = c("vcd_dat"),
    w13d4_vc_arm_1 = c("vcd_dat"),
    w14d1_vc_arm_1 = c("vcd_dat"),
    w14d2_vc_arm_1 = c("vcd_dat"),
    w14d3_vc_arm_1 = c("vcd_dat"),
    w14d4_vc_arm_1 = c("vcd_dat"),
    w15d1_vc_arm_1 = c("vcd_dat"),
    w15d2_vc_arm_1 = c("vcd_dat"),
    w15d3_vc_arm_1 = c("vcd_dat"),
    w15d4_vc_arm_1 = c("vcd_dat"),
    w16d1_vc_arm_1 = c("vcd_dat"),
    w16d2_vc_arm_1 = c("vcd_dat"),
    w16d3_vc_arm_1 = c("vcd_dat"),
    w16d4_vc_arm_1 = c("vcd_dat"),
    w17d1_vc_arm_1 = c("vcd_dat"),
    w17d2_vc_arm_1 = c("vcd_dat"),
    w17d3_vc_arm_1 = c("vcd_dat"),
    w17d4_vc_arm_1 = c("vcd_dat"),
    w18d1_vc_arm_1 = c("vcd_dat"),
    w18d2_vc_arm_1 = c("vcd_dat"),
    w18d3_vc_arm_1 = c("vcd_dat"),
    w18d4_vc_arm_1 = c("vcd_dat"),
    w19d1_vc_arm_1 = c("vcd_dat"),
    w19d2_vc_arm_1 = c("vcd_dat"),
    w19d3_vc_arm_1 = c("vcd_dat"),
    w19d4_vc_arm_1 = c("vcd_dat"),
    w20d1_vc_arm_1 = c("vcd_dat"),
    w20d2_vc_arm_1 = c("vcd_dat"),
    w20d3_vc_arm_1 = c("vcd_dat"),
    w20d4_vc_arm_1 = c("vcd_dat"),
    w21d1_vc_arm_1 = c("vcd_dat"),
    w21d2_vc_arm_1 = c("vcd_dat"),
    w21d3_vc_arm_1 = c("vcd_dat"),
    w21d4_vc_arm_1 = c("vcd_dat"),
    w22d1_vc_arm_1 = c("vcd_dat"),
    w22d2_vc_arm_1 = c("vcd_dat"),
    w22d3_vc_arm_1 = c("vcd_dat"),
    w22d4_vc_arm_1 = c("vcd_dat"),
    w23d1_vc_arm_1 = c("vcd_dat"),
    w23d2_vc_arm_1 = c("vcd_dat"),
    w23d3_vc_arm_1 = c("vcd_dat"),
    w23d4_vc_arm_1 = c("vcd_dat"),
    w24d1_vc_arm_1 = c("vcd_dat"),
    w24d2_vc_arm_1 = c("vcd_dat"),
    w24d3_vc_arm_1 = c("vcd_dat"),
    w24d4_vc_arm_1 = c("vcd_dat"),
    w25d1_vc_arm_1 = c("vcd_dat"),
    w25d2_vc_arm_1 = c("vcd_dat"),
    w25d3_vc_arm_1 = c("vcd_dat"),
    w25d4_vc_arm_1 = c("vcd_dat"),
    w26d1_vc_arm_1 = c("vcd_dat"),
    w26d2_vc_arm_1 = c("vcd_dat"),
    w26d3_vc_arm_1 = c("vcd_dat"),
    w26d4_vc_arm_1 = c("vcd_dat"),
    w27d1_vc_arm_1 = c("vcd_dat"),
    w27d2_vc_arm_1 = c("vcd_dat"),
    w27d3_vc_arm_1 = c("vcd_dat"),
    w27d4_vc_arm_1 = c("vcd_dat"),
    w28d1_vc_arm_1 = c("vcd_dat"),
    w28d2_vc_arm_1 = c("vcd_dat"),
    w28d3_vc_arm_1 = c("vcd_dat"),
    w28d4_vc_arm_1 = c("vcd_dat"),
    w29d1_vc_arm_1 = c("vcd_dat"),
    w29d2_vc_arm_1 = c("vcd_dat"),
    w29d3_vc_arm_1 = c("vcd_dat"),
    w29d4_vc_arm_1 = c("vcd_dat"),
    w30d1_vc_arm_1 = c("vcd_dat"),
    w30d2_vc_arm_1 = c("vcd_dat"),
    w30d3_vc_arm_1 = c("vcd_dat"),
    w30d4_vc_arm_1 = c("vcd_dat"),
    w31d1_vc_arm_1 = c("vcd_dat"),
    w31d2_vc_arm_1 = c("vcd_dat"),
    w31d3_vc_arm_1 = c("vcd_dat"),
    w31d4_vc_arm_1 = c("vcd_dat"),
    w32d1_vc_arm_1 = c("vcd_dat"),
    w32d2_vc_arm_1 = c("vcd_dat"),
    w32d3_vc_arm_1 = c("vcd_dat"),
    w32d4_vc_arm_1 = c("vcd_dat"),
    w33d1_vc_arm_1 = c("vcd_dat"),
    w33d2_vc_arm_1 = c("vcd_dat"),
    w33d3_vc_arm_1 = c("vcd_dat"),
    w33d4_vc_arm_1 = c("vcd_dat"),
    w34d1_vc_arm_1 = c("vcd_dat"),
    w34d2_vc_arm_1 = c("vcd_dat"),
    w34d3_vc_arm_1 = c("vcd_dat"),
    w34d4_vc_arm_1 = c("vcd_dat"),
    w35d1_vc_arm_1 = c("vcd_dat"),
    w35d2_vc_arm_1 = c("vcd_dat"),
    w35d3_vc_arm_1 = c("vcd_dat"),
    w35d4_vc_arm_1 = c("vcd_dat"),
    w36d1_vc_arm_1 = c("vcd_dat"),
    w36d2_vc_arm_1 = c("vcd_dat"),
    w36d3_vc_arm_1 = c("vcd_dat"),
    w36d4_vc_arm_1 = c("vcd_dat"),
    w37d1_vc_arm_1 = c("vcd_dat"),
    w37d2_vc_arm_1 = c("vcd_dat"),
    w37d3_vc_arm_1 = c("vcd_dat"),
    w37d4_vc_arm_1 = c("vcd_dat"),
    w38d1_vc_arm_1 = c("vcd_dat"),
    w38d2_vc_arm_1 = c("vcd_dat"),
    w38d3_vc_arm_1 = c("vcd_dat"),
    w38d4_vc_arm_1 = c("vcd_dat"),
    w39d1_vc_arm_1 = c("vcd_dat"),
    w39d2_vc_arm_1 = c("vcd_dat"),
    w39d3_vc_arm_1 = c("vcd_dat"),
    w39d4_vc_arm_1 = c("vcd_dat"),
    w40d1_vc_arm_1 = c("vcd_dat"),
    w40d2_vc_arm_1 = c("vcd_dat"),
    w40d3_vc_arm_1 = c("vcd_dat"),
    w40d4_vc_arm_1 = c("vcd_dat"),
    w41d1_vc_arm_1 = c("vcd_dat"),
    w41d2_vc_arm_1 = c("vcd_dat"),
    w41d3_vc_arm_1 = c("vcd_dat"),
    w41d4_vc_arm_1 = c("vcd_dat"),
    w42d1_vc_arm_1 = c("vcd_dat"),
    w42d2_vc_arm_1 = c("vcd_dat"),
    w42d3_vc_arm_1 = c("vcd_dat"),
    w42d4_vc_arm_1 = c("vcd_dat"),
    w43d1_vc_arm_1 = c("vcd_dat"),
    w43d2_vc_arm_1 = c("vcd_dat"),
    w43d3_vc_arm_1 = c("vcd_dat"),
    w43d4_vc_arm_1 = c("vcd_dat"),
    w44d1_vc_arm_1 = c("vcd_dat"),
    w44d2_vc_arm_1 = c("vcd_dat"),
    w44d3_vc_arm_1 = c("vcd_dat"),
    w44d4_vc_arm_1 = c("vcd_dat"),
    w45d1_vc_arm_1 = c("vcd_dat"),
    w45d2_vc_arm_1 = c("vcd_dat"),
    w45d3_vc_arm_1 = c("vcd_dat"),
    w45d4_vc_arm_1 = c("vcd_dat"),
    w46d1_vc_arm_1 = c("vcd_dat"),
    w46d2_vc_arm_1 = c("vcd_dat"),
    w46d3_vc_arm_1 = c("vcd_dat"),
    w46d4_vc_arm_1 = c("vcd_dat"),
    w47d1_vc_arm_1 = c("vcd_dat"),
    w47d2_vc_arm_1 = c("vcd_dat"),
    w47d3_vc_arm_1 = c("vcd_dat"),
    w47d4_vc_arm_1 = c("vcd_dat"),
    w48d1_vc_arm_1 = c("vcd_dat"),
    w48d2_vc_arm_1 = c("vcd_dat"),
    w48d3_vc_arm_1 = c("vcd_dat"),
    w48d4_vc_arm_1 = c("vcd_dat")
  )

stages_eq_two <- 
  list(
    # telephone screening
    scrn_tel_arm_1 = c("telephone_screening_complete"),
    # screening visit
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
    # baseline visit 1
    bv1_arm_1      = c("nacc_b4_cdr_complete",
                       "nacc_b1_physical_evaluation_complete",
                       "nacc_c2_neuropsych_scores_complete",
                       "neoffi_personality_inventory_complete"),
    # baseline clinician dx
    bl_cdx_arm_1   = c("nacc_d1_clinician_diagnosis_complete"),
    # baseline visit 2
    bv2_arm_1      = c("nih_toolbox_complete",
                       "otdlr_administration_complete",
                       "otdlr_composite_scores_complete",
                       "family_history_of_dementia_complete",
                       "apoe_complete"),
    # randomization
    admin_arm_1    = c("video_chat_randomization_form_complete"),
    # baseline MRI
    bl_mri_arm_1   = c("mri_scheduling_form_complete"),
    # weekly phone calls
    w01_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w02_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w03_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w04_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w05_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w06_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w07_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w08_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w09_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w10_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w11_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w12_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w13_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w14_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w15_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w16_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w17_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w18_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w19_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w20_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w21_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w22_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w23_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w24_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w25_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w26_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w27_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w28_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w29_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w30_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w31_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w32_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w33_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w34_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w35_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w36_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w37_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w38_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w39_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w40_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w41_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w42_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w43_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w44_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w45_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w46_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w47_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    w48_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),
    # daily video chats
    w01d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w01d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w01d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w01d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w02d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w02d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w02d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w02d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w03d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w03d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w03d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w03d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w04d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w04d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w04d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w04d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w05d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w05d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w05d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w05d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w06d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w06d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w06d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w06d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w07d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w07d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w07d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w07d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w08d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w08d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w08d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w08d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w09d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w09d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w09d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w09d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w10d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w10d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w10d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w10d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w11d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w11d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w11d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w11d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w12d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w12d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w12d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w12d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w13d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w13d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w13d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w13d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w14d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w14d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w14d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w14d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w15d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w15d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w15d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w15d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w16d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w16d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w16d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w16d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w17d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w17d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w17d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w17d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w18d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w18d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w18d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w18d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w19d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w19d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w19d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w19d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w20d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w20d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w20d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w20d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w21d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w21d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w21d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w21d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w22d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w22d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w22d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w22d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w23d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w23d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w23d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w23d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w24d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w24d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w24d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w24d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w25d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w25d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w25d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w25d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w26d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w26d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w26d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w26d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w27d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w27d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w27d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w27d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w28d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w28d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w28d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w28d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w29d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w29d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w29d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w29d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w30d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w30d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w30d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w30d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w31d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w31d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w31d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w31d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w32d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w32d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w32d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w32d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w33d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w33d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w33d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w33d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w34d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w34d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w34d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w34d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w35d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w35d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w35d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w35d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w36d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w36d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w36d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w36d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w37d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w37d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w37d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w37d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w38d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w38d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w38d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w38d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w39d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w39d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w39d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w39d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w40d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w40d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w40d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w40d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w41d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w41d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w41d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w41d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w42d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w42d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w42d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w42d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w43d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w43d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w43d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w43d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w44d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w44d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w44d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w44d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w45d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w45d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w45d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w45d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w46d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w46d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w46d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w46d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w47d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w47d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w47d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w47d4_vc_arm_1 = c("video_chat_daily_form_complete"),
    w48d1_vc_arm_1 = c("video_chat_daily_form_complete"),
    w48d2_vc_arm_1 = c("video_chat_daily_form_complete"),
    w48d3_vc_arm_1 = c("video_chat_daily_form_complete"),
    w48d4_vc_arm_1 = c("video_chat_daily_form_complete")
  )

# Get a specific value from the supplied dataframe
get_value <- function(uniq_id, redcap_event_name, field, df) {
  value = unlist(
    df[df$ts_sub_id == uniq_id &
         df$redcap_event_name == redcap_event_name,
       field]
  )
  
  ifelse(length(value) > 0, value, NA_character_)
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
  
  # # Pt. name
  # ts_lfn = get_value(uniq_id, "scrn_tel_arm_1", "ts_lfn", df)
  # ts_pfn = get_value(uniq_id, "scrn_tel_arm_1", "ts_pfn", df)
  # ts_lln = get_value(uniq_id, "scrn_tel_arm_1", "ts_lln", df)
  
  # telephone screening complete?
  scrn_tel_date = date_fields_complete(uniq_id, "scrn_tel_arm_1", df)
  scrn_tel_comp = form_fields_complete(uniq_id, "scrn_tel_arm_1", df)
  
  # screening visit complete?
  scrn_v_date = date_fields_complete(uniq_id, "scrn_v_arm_1", df)
  scrn_v_comp = form_fields_complete(uniq_id, "scrn_v_arm_1", df)
  
  # baseline visit 1 complete?
  bv1_date = date_fields_complete(uniq_id, "bv1_arm_1", df)
  bv1_comp = form_fields_complete(uniq_id, "bv1_arm_1", df)
  
  # basline diagnosis complete?
  bl_cdx_date = date_fields_complete(uniq_id, "bl_cdx_arm_1", df)
  bl_cdx_comp = form_fields_complete(uniq_id, "bl_cdx_arm_1", df)
  
  # baseline visit 2 complete?
  bv2_date = date_fields_complete(uniq_id, "bv2_arm_1", df)
  bv2_comp = form_fields_complete(uniq_id, "bv2_arm_1", df)
  
  # video chat randomization complete?
  admin_date = NA
  admin_comp = form_fields_complete(uniq_id, "admin_arm_1", df)
  
  # baseline mri scheduling complete?
  bl_mri_date = date_fields_complete(uniq_id, "bl_mri_arm_1", df)
  bl_mri_comp = form_fields_complete(uniq_id, "bl_mri_arm_1", df)
  
  # week 01 tel
  w01_tel_date = date_fields_complete(uniq_id, "w01_tel_arm_1", df)
  w01_tel_comp = form_fields_complete(uniq_id, "w01_tel_arm_1", df)
  # week 02 tel
  w02_tel_date = date_fields_complete(uniq_id, "w02_tel_arm_1", df)
  w02_tel_comp = form_fields_complete(uniq_id, "w02_tel_arm_1", df)
  # week 03 tel
  w03_tel_date = date_fields_complete(uniq_id, "w03_tel_arm_1", df)
  w03_tel_comp = form_fields_complete(uniq_id, "w03_tel_arm_1", df)
  # week 04 tel
  w04_tel_date = date_fields_complete(uniq_id, "w04_tel_arm_1", df)
  w04_tel_comp = form_fields_complete(uniq_id, "w04_tel_arm_1", df)
  # week 05 tel
  w05_tel_date = date_fields_complete(uniq_id, "w05_tel_arm_1", df)
  w05_tel_comp = form_fields_complete(uniq_id, "w05_tel_arm_1", df)
  # week 06 tel
  w06_tel_date = date_fields_complete(uniq_id, "w06_tel_arm_1", df)
  w06_tel_comp = form_fields_complete(uniq_id, "w06_tel_arm_1", df)
  # week 07 tel
  w07_tel_date = date_fields_complete(uniq_id, "w07_tel_arm_1", df)
  w07_tel_comp = form_fields_complete(uniq_id, "w07_tel_arm_1", df)
  # week 08 tel
  w08_tel_date = date_fields_complete(uniq_id, "w08_tel_arm_1", df)
  w08_tel_comp = form_fields_complete(uniq_id, "w08_tel_arm_1", df)
  # week 09 tel
  w09_tel_date = date_fields_complete(uniq_id, "w09_tel_arm_1", df)
  w09_tel_comp = form_fields_complete(uniq_id, "w09_tel_arm_1", df)
  # week 10 tel
  w10_tel_date = date_fields_complete(uniq_id, "w10_tel_arm_1", df)
  w10_tel_comp = form_fields_complete(uniq_id, "w10_tel_arm_1", df)
  # week 11 tel
  w11_tel_date = date_fields_complete(uniq_id, "w11_tel_arm_1", df)
  w11_tel_comp = form_fields_complete(uniq_id, "w11_tel_arm_1", df)
  # week 12 tel
  w12_tel_date = date_fields_complete(uniq_id, "w12_tel_arm_1", df)
  w12_tel_comp = form_fields_complete(uniq_id, "w12_tel_arm_1", df)
  # week 13 tel
  w13_tel_date = date_fields_complete(uniq_id, "w13_tel_arm_1", df)
  w13_tel_comp = form_fields_complete(uniq_id, "w13_tel_arm_1", df)
  # week 14 tel
  w14_tel_date = date_fields_complete(uniq_id, "w14_tel_arm_1", df)
  w14_tel_comp = form_fields_complete(uniq_id, "w14_tel_arm_1", df)
  # week 15 tel
  w15_tel_date = date_fields_complete(uniq_id, "w15_tel_arm_1", df)
  w15_tel_comp = form_fields_complete(uniq_id, "w15_tel_arm_1", df)
  # week 16 tel
  w16_tel_date = date_fields_complete(uniq_id, "w16_tel_arm_1", df)
  w16_tel_comp = form_fields_complete(uniq_id, "w16_tel_arm_1", df)
  # week 17 tel
  w17_tel_date = date_fields_complete(uniq_id, "w17_tel_arm_1", df)
  w17_tel_comp = form_fields_complete(uniq_id, "w17_tel_arm_1", df)
  # week 18 tel
  w18_tel_date = date_fields_complete(uniq_id, "w18_tel_arm_1", df)
  w18_tel_comp = form_fields_complete(uniq_id, "w18_tel_arm_1", df)
  # week 19 tel
  w19_tel_date = date_fields_complete(uniq_id, "w19_tel_arm_1", df)
  w19_tel_comp = form_fields_complete(uniq_id, "w19_tel_arm_1", df)
  # week 20 tel
  w20_tel_date = date_fields_complete(uniq_id, "w20_tel_arm_1", df)
  w20_tel_comp = form_fields_complete(uniq_id, "w20_tel_arm_1", df)
  # week 21 tel
  w21_tel_date = date_fields_complete(uniq_id, "w21_tel_arm_1", df)
  w21_tel_comp = form_fields_complete(uniq_id, "w21_tel_arm_1", df)
  # week 22 tel
  w22_tel_date = date_fields_complete(uniq_id, "w22_tel_arm_1", df)
  w22_tel_comp = form_fields_complete(uniq_id, "w22_tel_arm_1", df)
  # week 23 tel
  w23_tel_date = date_fields_complete(uniq_id, "w23_tel_arm_1", df)
  w23_tel_comp = form_fields_complete(uniq_id, "w23_tel_arm_1", df)
  # week 24 tel
  w24_tel_date = date_fields_complete(uniq_id, "w24_tel_arm_1", df)
  w24_tel_comp = form_fields_complete(uniq_id, "w24_tel_arm_1", df)
  # week 25 tel
  w25_tel_date = date_fields_complete(uniq_id, "w25_tel_arm_1", df)
  w25_tel_comp = form_fields_complete(uniq_id, "w25_tel_arm_1", df)
  # week 26 tel
  w26_tel_date = date_fields_complete(uniq_id, "w26_tel_arm_1", df)
  w26_tel_comp = form_fields_complete(uniq_id, "w26_tel_arm_1", df)
  # week 27 tel
  w27_tel_date = date_fields_complete(uniq_id, "w27_tel_arm_1", df)
  w27_tel_comp = form_fields_complete(uniq_id, "w27_tel_arm_1", df)
  # week 28 tel
  w28_tel_date = date_fields_complete(uniq_id, "w28_tel_arm_1", df)
  w28_tel_comp = form_fields_complete(uniq_id, "w28_tel_arm_1", df)
  # week 29 tel
  w29_tel_date = date_fields_complete(uniq_id, "w29_tel_arm_1", df)
  w29_tel_comp = form_fields_complete(uniq_id, "w29_tel_arm_1", df)
  # week 30 tel
  w30_tel_date = date_fields_complete(uniq_id, "w30_tel_arm_1", df)
  w30_tel_comp = form_fields_complete(uniq_id, "w30_tel_arm_1", df)
  # week 31 tel
  w31_tel_date = date_fields_complete(uniq_id, "w31_tel_arm_1", df)
  w31_tel_comp = form_fields_complete(uniq_id, "w31_tel_arm_1", df)
  # week 32 tel
  w32_tel_date = date_fields_complete(uniq_id, "w32_tel_arm_1", df)
  w32_tel_comp = form_fields_complete(uniq_id, "w32_tel_arm_1", df)
  # week 33 tel
  w33_tel_date = date_fields_complete(uniq_id, "w33_tel_arm_1", df)
  w33_tel_comp = form_fields_complete(uniq_id, "w33_tel_arm_1", df)
  # week 34 tel
  w34_tel_date = date_fields_complete(uniq_id, "w34_tel_arm_1", df)
  w34_tel_comp = form_fields_complete(uniq_id, "w34_tel_arm_1", df)
  # week 35 tel
  w35_tel_date = date_fields_complete(uniq_id, "w35_tel_arm_1", df)
  w35_tel_comp = form_fields_complete(uniq_id, "w35_tel_arm_1", df)
  # week 36 tel
  w36_tel_date = date_fields_complete(uniq_id, "w36_tel_arm_1", df)
  w36_tel_comp = form_fields_complete(uniq_id, "w36_tel_arm_1", df)
  # week 37 tel
  w37_tel_date = date_fields_complete(uniq_id, "w37_tel_arm_1", df)
  w37_tel_comp = form_fields_complete(uniq_id, "w37_tel_arm_1", df)
  # week 38 tel
  w38_tel_date = date_fields_complete(uniq_id, "w38_tel_arm_1", df)
  w38_tel_comp = form_fields_complete(uniq_id, "w38_tel_arm_1", df)
  # week 39 tel
  w39_tel_date = date_fields_complete(uniq_id, "w39_tel_arm_1", df)
  w39_tel_comp = form_fields_complete(uniq_id, "w39_tel_arm_1", df)
  # week 40 tel
  w40_tel_date = date_fields_complete(uniq_id, "w40_tel_arm_1", df)
  w40_tel_comp = form_fields_complete(uniq_id, "w40_tel_arm_1", df)
  # week 41 tel
  w41_tel_date = date_fields_complete(uniq_id, "w41_tel_arm_1", df)
  w41_tel_comp = form_fields_complete(uniq_id, "w41_tel_arm_1", df)
  # week 42 tel
  w42_tel_date = date_fields_complete(uniq_id, "w42_tel_arm_1", df)
  w42_tel_comp = form_fields_complete(uniq_id, "w42_tel_arm_1", df)
  # week 43 tel
  w43_tel_date = date_fields_complete(uniq_id, "w43_tel_arm_1", df)
  w43_tel_comp = form_fields_complete(uniq_id, "w43_tel_arm_1", df)
  # week 44 tel
  w44_tel_date = date_fields_complete(uniq_id, "w44_tel_arm_1", df)
  w44_tel_comp = form_fields_complete(uniq_id, "w44_tel_arm_1", df)
  # week 45 tel
  w45_tel_date = date_fields_complete(uniq_id, "w45_tel_arm_1", df)
  w45_tel_comp = form_fields_complete(uniq_id, "w45_tel_arm_1", df)
  # week 46 tel
  w46_tel_date = date_fields_complete(uniq_id, "w46_tel_arm_1", df)
  w46_tel_comp = form_fields_complete(uniq_id, "w46_tel_arm_1", df)
  # week 47 tel
  w47_tel_date = date_fields_complete(uniq_id, "w47_tel_arm_1", df)
  w47_tel_comp = form_fields_complete(uniq_id, "w47_tel_arm_1", df)
  # week 48 tel
  w48_tel_date = date_fields_complete(uniq_id, "w48_tel_arm_1", df)
  w48_tel_comp = form_fields_complete(uniq_id, "w48_tel_arm_1", df)
  
  # week 01 video chat
  w01d1_vc_date = date_fields_complete(uniq_id, "w01d1_vc_arm_1", df)
  w01d2_vc_date = date_fields_complete(uniq_id, "w01d2_vc_arm_1", df)
  w01d3_vc_date = date_fields_complete(uniq_id, "w01d3_vc_arm_1", df)
  w01d4_vc_date = date_fields_complete(uniq_id, "w01d4_vc_arm_1", df)
  w01d1_vc_comp = form_fields_complete(uniq_id, "w01d1_vc_arm_1", df)
  w01d2_vc_comp = form_fields_complete(uniq_id, "w01d2_vc_arm_1", df)
  w01d3_vc_comp = form_fields_complete(uniq_id, "w01d3_vc_arm_1", df)
  w01d4_vc_comp = form_fields_complete(uniq_id, "w01d4_vc_arm_1", df)
  # week 02 video chat
  w02d1_vc_date = date_fields_complete(uniq_id, "w02d1_vc_arm_1", df)
  w02d2_vc_date = date_fields_complete(uniq_id, "w02d2_vc_arm_1", df)
  w02d3_vc_date = date_fields_complete(uniq_id, "w02d3_vc_arm_1", df)
  w02d4_vc_date = date_fields_complete(uniq_id, "w02d4_vc_arm_1", df)
  w02d1_vc_comp = form_fields_complete(uniq_id, "w02d1_vc_arm_1", df)
  w02d2_vc_comp = form_fields_complete(uniq_id, "w02d2_vc_arm_1", df)
  w02d3_vc_comp = form_fields_complete(uniq_id, "w02d3_vc_arm_1", df)
  w02d4_vc_comp = form_fields_complete(uniq_id, "w02d4_vc_arm_1", df)
  # week 03 video chat
  w03d1_vc_date = date_fields_complete(uniq_id, "w03d1_vc_arm_1", df)
  w03d2_vc_date = date_fields_complete(uniq_id, "w03d2_vc_arm_1", df)
  w03d3_vc_date = date_fields_complete(uniq_id, "w03d3_vc_arm_1", df)
  w03d4_vc_date = date_fields_complete(uniq_id, "w03d4_vc_arm_1", df)
  w03d1_vc_comp = form_fields_complete(uniq_id, "w03d1_vc_arm_1", df)
  w03d2_vc_comp = form_fields_complete(uniq_id, "w03d2_vc_arm_1", df)
  w03d3_vc_comp = form_fields_complete(uniq_id, "w03d3_vc_arm_1", df)
  w03d4_vc_comp = form_fields_complete(uniq_id, "w03d4_vc_arm_1", df)
  # week 04 video chat
  w04d1_vc_date = date_fields_complete(uniq_id, "w04d1_vc_arm_1", df)
  w04d2_vc_date = date_fields_complete(uniq_id, "w04d2_vc_arm_1", df)
  w04d3_vc_date = date_fields_complete(uniq_id, "w04d3_vc_arm_1", df)
  w04d4_vc_date = date_fields_complete(uniq_id, "w04d4_vc_arm_1", df)
  w04d1_vc_comp = form_fields_complete(uniq_id, "w04d1_vc_arm_1", df)
  w04d2_vc_comp = form_fields_complete(uniq_id, "w04d2_vc_arm_1", df)
  w04d3_vc_comp = form_fields_complete(uniq_id, "w04d3_vc_arm_1", df)
  w04d4_vc_comp = form_fields_complete(uniq_id, "w04d4_vc_arm_1", df)
  # week 05 video chat
  w05d1_vc_date = date_fields_complete(uniq_id, "w05d1_vc_arm_1", df)
  w05d2_vc_date = date_fields_complete(uniq_id, "w05d2_vc_arm_1", df)
  w05d3_vc_date = date_fields_complete(uniq_id, "w05d3_vc_arm_1", df)
  w05d4_vc_date = date_fields_complete(uniq_id, "w05d4_vc_arm_1", df)
  w05d1_vc_comp = form_fields_complete(uniq_id, "w05d1_vc_arm_1", df)
  w05d2_vc_comp = form_fields_complete(uniq_id, "w05d2_vc_arm_1", df)
  w05d3_vc_comp = form_fields_complete(uniq_id, "w05d3_vc_arm_1", df)
  w05d4_vc_comp = form_fields_complete(uniq_id, "w05d4_vc_arm_1", df)
  # week 06 video chat
  w06d1_vc_date = date_fields_complete(uniq_id, "w06d1_vc_arm_1", df)
  w06d2_vc_date = date_fields_complete(uniq_id, "w06d2_vc_arm_1", df)
  w06d3_vc_date = date_fields_complete(uniq_id, "w06d3_vc_arm_1", df)
  w06d4_vc_date = date_fields_complete(uniq_id, "w06d4_vc_arm_1", df)
  w06d1_vc_comp = form_fields_complete(uniq_id, "w06d1_vc_arm_1", df)
  w06d2_vc_comp = form_fields_complete(uniq_id, "w06d2_vc_arm_1", df)
  w06d3_vc_comp = form_fields_complete(uniq_id, "w06d3_vc_arm_1", df)
  w06d4_vc_comp = form_fields_complete(uniq_id, "w06d4_vc_arm_1", df)
  # week 07 video chat
  w07d1_vc_date = date_fields_complete(uniq_id, "w07d1_vc_arm_1", df)
  w07d2_vc_date = date_fields_complete(uniq_id, "w07d2_vc_arm_1", df)
  w07d3_vc_date = date_fields_complete(uniq_id, "w07d3_vc_arm_1", df)
  w07d4_vc_date = date_fields_complete(uniq_id, "w07d4_vc_arm_1", df)
  w07d1_vc_comp = form_fields_complete(uniq_id, "w07d1_vc_arm_1", df)
  w07d2_vc_comp = form_fields_complete(uniq_id, "w07d2_vc_arm_1", df)
  w07d3_vc_comp = form_fields_complete(uniq_id, "w07d3_vc_arm_1", df)
  w07d4_vc_comp = form_fields_complete(uniq_id, "w07d4_vc_arm_1", df)
  # week 08 video chat
  w08d1_vc_date = date_fields_complete(uniq_id, "w08d1_vc_arm_1", df)
  w08d2_vc_date = date_fields_complete(uniq_id, "w08d2_vc_arm_1", df)
  w08d3_vc_date = date_fields_complete(uniq_id, "w08d3_vc_arm_1", df)
  w08d4_vc_date = date_fields_complete(uniq_id, "w08d4_vc_arm_1", df)
  w08d1_vc_comp = form_fields_complete(uniq_id, "w08d1_vc_arm_1", df)
  w08d2_vc_comp = form_fields_complete(uniq_id, "w08d2_vc_arm_1", df)
  w08d3_vc_comp = form_fields_complete(uniq_id, "w08d3_vc_arm_1", df)
  w08d4_vc_comp = form_fields_complete(uniq_id, "w08d4_vc_arm_1", df)
  # week 09 video chat
  w09d1_vc_date = date_fields_complete(uniq_id, "w09d1_vc_arm_1", df)
  w09d2_vc_date = date_fields_complete(uniq_id, "w09d2_vc_arm_1", df)
  w09d3_vc_date = date_fields_complete(uniq_id, "w09d3_vc_arm_1", df)
  w09d4_vc_date = date_fields_complete(uniq_id, "w09d4_vc_arm_1", df)
  w09d1_vc_comp = form_fields_complete(uniq_id, "w09d1_vc_arm_1", df)
  w09d2_vc_comp = form_fields_complete(uniq_id, "w09d2_vc_arm_1", df)
  w09d3_vc_comp = form_fields_complete(uniq_id, "w09d3_vc_arm_1", df)
  w09d4_vc_comp = form_fields_complete(uniq_id, "w09d4_vc_arm_1", df)
  # week 010 video chat
  w10d1_vc_date = date_fields_complete(uniq_id, "w10d1_vc_arm_1", df)
  w10d2_vc_date = date_fields_complete(uniq_id, "w10d2_vc_arm_1", df)
  w10d3_vc_date = date_fields_complete(uniq_id, "w10d3_vc_arm_1", df)
  w10d4_vc_date = date_fields_complete(uniq_id, "w10d4_vc_arm_1", df)
  w10d1_vc_comp = form_fields_complete(uniq_id, "w10d1_vc_arm_1", df)
  w10d2_vc_comp = form_fields_complete(uniq_id, "w10d2_vc_arm_1", df)
  w10d3_vc_comp = form_fields_complete(uniq_id, "w10d3_vc_arm_1", df)
  w10d4_vc_comp = form_fields_complete(uniq_id, "w10d4_vc_arm_1", df)
  # week 011 video chat
  w11d1_vc_date = date_fields_complete(uniq_id, "w11d1_vc_arm_1", df)
  w11d2_vc_date = date_fields_complete(uniq_id, "w11d2_vc_arm_1", df)
  w11d3_vc_date = date_fields_complete(uniq_id, "w11d3_vc_arm_1", df)
  w11d4_vc_date = date_fields_complete(uniq_id, "w11d4_vc_arm_1", df)
  w11d1_vc_comp = form_fields_complete(uniq_id, "w11d1_vc_arm_1", df)
  w11d2_vc_comp = form_fields_complete(uniq_id, "w11d2_vc_arm_1", df)
  w11d3_vc_comp = form_fields_complete(uniq_id, "w11d3_vc_arm_1", df)
  w11d4_vc_comp = form_fields_complete(uniq_id, "w11d4_vc_arm_1", df)
  # week 012 video chat
  w12d1_vc_date = date_fields_complete(uniq_id, "w12d1_vc_arm_1", df)
  w12d2_vc_date = date_fields_complete(uniq_id, "w12d2_vc_arm_1", df)
  w12d3_vc_date = date_fields_complete(uniq_id, "w12d3_vc_arm_1", df)
  w12d4_vc_date = date_fields_complete(uniq_id, "w12d4_vc_arm_1", df)
  w12d1_vc_comp = form_fields_complete(uniq_id, "w12d1_vc_arm_1", df)
  w12d2_vc_comp = form_fields_complete(uniq_id, "w12d2_vc_arm_1", df)
  w12d3_vc_comp = form_fields_complete(uniq_id, "w12d3_vc_arm_1", df)
  w12d4_vc_comp = form_fields_complete(uniq_id, "w12d4_vc_arm_1", df)
  # week 013 video chat
  w13d1_vc_date = date_fields_complete(uniq_id, "w13d1_vc_arm_1", df)
  w13d2_vc_date = date_fields_complete(uniq_id, "w13d2_vc_arm_1", df)
  w13d3_vc_date = date_fields_complete(uniq_id, "w13d3_vc_arm_1", df)
  w13d4_vc_date = date_fields_complete(uniq_id, "w13d4_vc_arm_1", df)
  w13d1_vc_comp = form_fields_complete(uniq_id, "w13d1_vc_arm_1", df)
  w13d2_vc_comp = form_fields_complete(uniq_id, "w13d2_vc_arm_1", df)
  w13d3_vc_comp = form_fields_complete(uniq_id, "w13d3_vc_arm_1", df)
  w13d4_vc_comp = form_fields_complete(uniq_id, "w13d4_vc_arm_1", df)
  # week 014 video chat
  w14d1_vc_date = date_fields_complete(uniq_id, "w14d1_vc_arm_1", df)
  w14d2_vc_date = date_fields_complete(uniq_id, "w14d2_vc_arm_1", df)
  w14d3_vc_date = date_fields_complete(uniq_id, "w14d3_vc_arm_1", df)
  w14d4_vc_date = date_fields_complete(uniq_id, "w14d4_vc_arm_1", df)
  w14d1_vc_comp = form_fields_complete(uniq_id, "w14d1_vc_arm_1", df)
  w14d2_vc_comp = form_fields_complete(uniq_id, "w14d2_vc_arm_1", df)
  w14d3_vc_comp = form_fields_complete(uniq_id, "w14d3_vc_arm_1", df)
  w14d4_vc_comp = form_fields_complete(uniq_id, "w14d4_vc_arm_1", df)
  # week 015 video chat
  w15d1_vc_date = date_fields_complete(uniq_id, "w15d1_vc_arm_1", df)
  w15d2_vc_date = date_fields_complete(uniq_id, "w15d2_vc_arm_1", df)
  w15d3_vc_date = date_fields_complete(uniq_id, "w15d3_vc_arm_1", df)
  w15d4_vc_date = date_fields_complete(uniq_id, "w15d4_vc_arm_1", df)
  w15d1_vc_comp = form_fields_complete(uniq_id, "w15d1_vc_arm_1", df)
  w15d2_vc_comp = form_fields_complete(uniq_id, "w15d2_vc_arm_1", df)
  w15d3_vc_comp = form_fields_complete(uniq_id, "w15d3_vc_arm_1", df)
  w15d4_vc_comp = form_fields_complete(uniq_id, "w15d4_vc_arm_1", df)
  # week 016 video chat
  w16d1_vc_date = date_fields_complete(uniq_id, "w16d1_vc_arm_1", df)
  w16d2_vc_date = date_fields_complete(uniq_id, "w16d2_vc_arm_1", df)
  w16d3_vc_date = date_fields_complete(uniq_id, "w16d3_vc_arm_1", df)
  w16d4_vc_date = date_fields_complete(uniq_id, "w16d4_vc_arm_1", df)
  w16d1_vc_comp = form_fields_complete(uniq_id, "w16d1_vc_arm_1", df)
  w16d2_vc_comp = form_fields_complete(uniq_id, "w16d2_vc_arm_1", df)
  w16d3_vc_comp = form_fields_complete(uniq_id, "w16d3_vc_arm_1", df)
  w16d4_vc_comp = form_fields_complete(uniq_id, "w16d4_vc_arm_1", df)
  # week 017 video chat
  w17d1_vc_date = date_fields_complete(uniq_id, "w17d1_vc_arm_1", df)
  w17d2_vc_date = date_fields_complete(uniq_id, "w17d2_vc_arm_1", df)
  w17d3_vc_date = date_fields_complete(uniq_id, "w17d3_vc_arm_1", df)
  w17d4_vc_date = date_fields_complete(uniq_id, "w17d4_vc_arm_1", df)
  w17d1_vc_comp = form_fields_complete(uniq_id, "w17d1_vc_arm_1", df)
  w17d2_vc_comp = form_fields_complete(uniq_id, "w17d2_vc_arm_1", df)
  w17d3_vc_comp = form_fields_complete(uniq_id, "w17d3_vc_arm_1", df)
  w17d4_vc_comp = form_fields_complete(uniq_id, "w17d4_vc_arm_1", df)
  # week 018 video chat
  w18d1_vc_date = date_fields_complete(uniq_id, "w18d1_vc_arm_1", df)
  w18d2_vc_date = date_fields_complete(uniq_id, "w18d2_vc_arm_1", df)
  w18d3_vc_date = date_fields_complete(uniq_id, "w18d3_vc_arm_1", df)
  w18d4_vc_date = date_fields_complete(uniq_id, "w18d4_vc_arm_1", df)
  w18d1_vc_comp = form_fields_complete(uniq_id, "w18d1_vc_arm_1", df)
  w18d2_vc_comp = form_fields_complete(uniq_id, "w18d2_vc_arm_1", df)
  w18d3_vc_comp = form_fields_complete(uniq_id, "w18d3_vc_arm_1", df)
  w18d4_vc_comp = form_fields_complete(uniq_id, "w18d4_vc_arm_1", df)
  # week 019 video chat
  w19d1_vc_date = date_fields_complete(uniq_id, "w19d1_vc_arm_1", df)
  w19d2_vc_date = date_fields_complete(uniq_id, "w19d2_vc_arm_1", df)
  w19d3_vc_date = date_fields_complete(uniq_id, "w19d3_vc_arm_1", df)
  w19d4_vc_date = date_fields_complete(uniq_id, "w19d4_vc_arm_1", df)
  w19d1_vc_comp = form_fields_complete(uniq_id, "w19d1_vc_arm_1", df)
  w19d2_vc_comp = form_fields_complete(uniq_id, "w19d2_vc_arm_1", df)
  w19d3_vc_comp = form_fields_complete(uniq_id, "w19d3_vc_arm_1", df)
  w19d4_vc_comp = form_fields_complete(uniq_id, "w19d4_vc_arm_1", df)
  # week 020 video chat
  w20d1_vc_date = date_fields_complete(uniq_id, "w20d1_vc_arm_1", df)
  w20d2_vc_date = date_fields_complete(uniq_id, "w20d2_vc_arm_1", df)
  w20d3_vc_date = date_fields_complete(uniq_id, "w20d3_vc_arm_1", df)
  w20d4_vc_date = date_fields_complete(uniq_id, "w20d4_vc_arm_1", df)
  w20d1_vc_comp = form_fields_complete(uniq_id, "w20d1_vc_arm_1", df)
  w20d2_vc_comp = form_fields_complete(uniq_id, "w20d2_vc_arm_1", df)
  w20d3_vc_comp = form_fields_complete(uniq_id, "w20d3_vc_arm_1", df)
  w20d4_vc_comp = form_fields_complete(uniq_id, "w20d4_vc_arm_1", df)
  # week 021 video chat
  w21d1_vc_date = date_fields_complete(uniq_id, "w21d1_vc_arm_1", df)
  w21d2_vc_date = date_fields_complete(uniq_id, "w21d2_vc_arm_1", df)
  w21d3_vc_date = date_fields_complete(uniq_id, "w21d3_vc_arm_1", df)
  w21d4_vc_date = date_fields_complete(uniq_id, "w21d4_vc_arm_1", df)
  w21d1_vc_comp = form_fields_complete(uniq_id, "w21d1_vc_arm_1", df)
  w21d2_vc_comp = form_fields_complete(uniq_id, "w21d2_vc_arm_1", df)
  w21d3_vc_comp = form_fields_complete(uniq_id, "w21d3_vc_arm_1", df)
  w21d4_vc_comp = form_fields_complete(uniq_id, "w21d4_vc_arm_1", df)
  # week 022 video chat
  w22d1_vc_date = date_fields_complete(uniq_id, "w22d1_vc_arm_1", df)
  w22d2_vc_date = date_fields_complete(uniq_id, "w22d2_vc_arm_1", df)
  w22d3_vc_date = date_fields_complete(uniq_id, "w22d3_vc_arm_1", df)
  w22d4_vc_date = date_fields_complete(uniq_id, "w22d4_vc_arm_1", df)
  w22d1_vc_comp = form_fields_complete(uniq_id, "w22d1_vc_arm_1", df)
  w22d2_vc_comp = form_fields_complete(uniq_id, "w22d2_vc_arm_1", df)
  w22d3_vc_comp = form_fields_complete(uniq_id, "w22d3_vc_arm_1", df)
  w22d4_vc_comp = form_fields_complete(uniq_id, "w22d4_vc_arm_1", df)
  # week 023 video chat
  w23d1_vc_date = date_fields_complete(uniq_id, "w23d1_vc_arm_1", df)
  w23d2_vc_date = date_fields_complete(uniq_id, "w23d2_vc_arm_1", df)
  w23d3_vc_date = date_fields_complete(uniq_id, "w23d3_vc_arm_1", df)
  w23d4_vc_date = date_fields_complete(uniq_id, "w23d4_vc_arm_1", df)
  w23d1_vc_comp = form_fields_complete(uniq_id, "w23d1_vc_arm_1", df)
  w23d2_vc_comp = form_fields_complete(uniq_id, "w23d2_vc_arm_1", df)
  w23d3_vc_comp = form_fields_complete(uniq_id, "w23d3_vc_arm_1", df)
  w23d4_vc_comp = form_fields_complete(uniq_id, "w23d4_vc_arm_1", df)
  # week 024 video chat
  w24d1_vc_date = date_fields_complete(uniq_id, "w24d1_vc_arm_1", df)
  w24d2_vc_date = date_fields_complete(uniq_id, "w24d2_vc_arm_1", df)
  w24d3_vc_date = date_fields_complete(uniq_id, "w24d3_vc_arm_1", df)
  w24d4_vc_date = date_fields_complete(uniq_id, "w24d4_vc_arm_1", df)
  w24d1_vc_comp = form_fields_complete(uniq_id, "w24d1_vc_arm_1", df)
  w24d2_vc_comp = form_fields_complete(uniq_id, "w24d2_vc_arm_1", df)
  w24d3_vc_comp = form_fields_complete(uniq_id, "w24d3_vc_arm_1", df)
  w24d4_vc_comp = form_fields_complete(uniq_id, "w24d4_vc_arm_1", df)
  # week 025 video chat
  w25d1_vc_date = date_fields_complete(uniq_id, "w25d1_vc_arm_1", df)
  w25d2_vc_date = date_fields_complete(uniq_id, "w25d2_vc_arm_1", df)
  w25d3_vc_date = date_fields_complete(uniq_id, "w25d3_vc_arm_1", df)
  w25d4_vc_date = date_fields_complete(uniq_id, "w25d4_vc_arm_1", df)
  w25d1_vc_comp = form_fields_complete(uniq_id, "w25d1_vc_arm_1", df)
  w25d2_vc_comp = form_fields_complete(uniq_id, "w25d2_vc_arm_1", df)
  w25d3_vc_comp = form_fields_complete(uniq_id, "w25d3_vc_arm_1", df)
  w25d4_vc_comp = form_fields_complete(uniq_id, "w25d4_vc_arm_1", df)
  # week 026 video chat
  w26d1_vc_date = date_fields_complete(uniq_id, "w26d1_vc_arm_1", df)
  w26d2_vc_date = date_fields_complete(uniq_id, "w26d2_vc_arm_1", df)
  w26d3_vc_date = date_fields_complete(uniq_id, "w26d3_vc_arm_1", df)
  w26d4_vc_date = date_fields_complete(uniq_id, "w26d4_vc_arm_1", df)
  w26d1_vc_comp = form_fields_complete(uniq_id, "w26d1_vc_arm_1", df)
  w26d2_vc_comp = form_fields_complete(uniq_id, "w26d2_vc_arm_1", df)
  w26d3_vc_comp = form_fields_complete(uniq_id, "w26d3_vc_arm_1", df)
  w26d4_vc_comp = form_fields_complete(uniq_id, "w26d4_vc_arm_1", df)
  # week 027 video chat
  w27d1_vc_date = date_fields_complete(uniq_id, "w27d1_vc_arm_1", df)
  w27d2_vc_date = date_fields_complete(uniq_id, "w27d2_vc_arm_1", df)
  w27d3_vc_date = date_fields_complete(uniq_id, "w27d3_vc_arm_1", df)
  w27d4_vc_date = date_fields_complete(uniq_id, "w27d4_vc_arm_1", df)
  w27d1_vc_comp = form_fields_complete(uniq_id, "w27d1_vc_arm_1", df)
  w27d2_vc_comp = form_fields_complete(uniq_id, "w27d2_vc_arm_1", df)
  w27d3_vc_comp = form_fields_complete(uniq_id, "w27d3_vc_arm_1", df)
  w27d4_vc_comp = form_fields_complete(uniq_id, "w27d4_vc_arm_1", df)
  # week 028 video chat
  w28d1_vc_date = date_fields_complete(uniq_id, "w28d1_vc_arm_1", df)
  w28d2_vc_date = date_fields_complete(uniq_id, "w28d2_vc_arm_1", df)
  w28d3_vc_date = date_fields_complete(uniq_id, "w28d3_vc_arm_1", df)
  w28d4_vc_date = date_fields_complete(uniq_id, "w28d4_vc_arm_1", df)
  w28d1_vc_comp = form_fields_complete(uniq_id, "w28d1_vc_arm_1", df)
  w28d2_vc_comp = form_fields_complete(uniq_id, "w28d2_vc_arm_1", df)
  w28d3_vc_comp = form_fields_complete(uniq_id, "w28d3_vc_arm_1", df)
  w28d4_vc_comp = form_fields_complete(uniq_id, "w28d4_vc_arm_1", df)
  # week 029 video chat
  w29d1_vc_date = date_fields_complete(uniq_id, "w29d1_vc_arm_1", df)
  w29d2_vc_date = date_fields_complete(uniq_id, "w29d2_vc_arm_1", df)
  w29d3_vc_date = date_fields_complete(uniq_id, "w29d3_vc_arm_1", df)
  w29d4_vc_date = date_fields_complete(uniq_id, "w29d4_vc_arm_1", df)
  w29d1_vc_comp = form_fields_complete(uniq_id, "w29d1_vc_arm_1", df)
  w29d2_vc_comp = form_fields_complete(uniq_id, "w29d2_vc_arm_1", df)
  w29d3_vc_comp = form_fields_complete(uniq_id, "w29d3_vc_arm_1", df)
  w29d4_vc_comp = form_fields_complete(uniq_id, "w29d4_vc_arm_1", df)
  # week 030 video chat
  w30d1_vc_date = date_fields_complete(uniq_id, "w30d1_vc_arm_1", df)
  w30d2_vc_date = date_fields_complete(uniq_id, "w30d2_vc_arm_1", df)
  w30d3_vc_date = date_fields_complete(uniq_id, "w30d3_vc_arm_1", df)
  w30d4_vc_date = date_fields_complete(uniq_id, "w30d4_vc_arm_1", df)
  w30d1_vc_comp = form_fields_complete(uniq_id, "w30d1_vc_arm_1", df)
  w30d2_vc_comp = form_fields_complete(uniq_id, "w30d2_vc_arm_1", df)
  w30d3_vc_comp = form_fields_complete(uniq_id, "w30d3_vc_arm_1", df)
  w30d4_vc_comp = form_fields_complete(uniq_id, "w30d4_vc_arm_1", df)
  # week 031 video chat
  w31d1_vc_date = date_fields_complete(uniq_id, "w31d1_vc_arm_1", df)
  w31d2_vc_date = date_fields_complete(uniq_id, "w31d2_vc_arm_1", df)
  w31d3_vc_date = date_fields_complete(uniq_id, "w31d3_vc_arm_1", df)
  w31d4_vc_date = date_fields_complete(uniq_id, "w31d4_vc_arm_1", df)
  w31d1_vc_comp = form_fields_complete(uniq_id, "w31d1_vc_arm_1", df)
  w31d2_vc_comp = form_fields_complete(uniq_id, "w31d2_vc_arm_1", df)
  w31d3_vc_comp = form_fields_complete(uniq_id, "w31d3_vc_arm_1", df)
  w31d4_vc_comp = form_fields_complete(uniq_id, "w31d4_vc_arm_1", df)
  # week 032 video chat
  w32d1_vc_date = date_fields_complete(uniq_id, "w32d1_vc_arm_1", df)
  w32d2_vc_date = date_fields_complete(uniq_id, "w32d2_vc_arm_1", df)
  w32d3_vc_date = date_fields_complete(uniq_id, "w32d3_vc_arm_1", df)
  w32d4_vc_date = date_fields_complete(uniq_id, "w32d4_vc_arm_1", df)
  w32d1_vc_comp = form_fields_complete(uniq_id, "w32d1_vc_arm_1", df)
  w32d2_vc_comp = form_fields_complete(uniq_id, "w32d2_vc_arm_1", df)
  w32d3_vc_comp = form_fields_complete(uniq_id, "w32d3_vc_arm_1", df)
  w32d4_vc_comp = form_fields_complete(uniq_id, "w32d4_vc_arm_1", df)
  # week 033 video chat
  w33d1_vc_date = date_fields_complete(uniq_id, "w33d1_vc_arm_1", df)
  w33d2_vc_date = date_fields_complete(uniq_id, "w33d2_vc_arm_1", df)
  w33d3_vc_date = date_fields_complete(uniq_id, "w33d3_vc_arm_1", df)
  w33d4_vc_date = date_fields_complete(uniq_id, "w33d4_vc_arm_1", df)
  w33d1_vc_comp = form_fields_complete(uniq_id, "w33d1_vc_arm_1", df)
  w33d2_vc_comp = form_fields_complete(uniq_id, "w33d2_vc_arm_1", df)
  w33d3_vc_comp = form_fields_complete(uniq_id, "w33d3_vc_arm_1", df)
  w33d4_vc_comp = form_fields_complete(uniq_id, "w33d4_vc_arm_1", df)
  # week 034 video chat
  w34d1_vc_date = date_fields_complete(uniq_id, "w34d1_vc_arm_1", df)
  w34d2_vc_date = date_fields_complete(uniq_id, "w34d2_vc_arm_1", df)
  w34d3_vc_date = date_fields_complete(uniq_id, "w34d3_vc_arm_1", df)
  w34d4_vc_date = date_fields_complete(uniq_id, "w34d4_vc_arm_1", df)
  w34d1_vc_comp = form_fields_complete(uniq_id, "w34d1_vc_arm_1", df)
  w34d2_vc_comp = form_fields_complete(uniq_id, "w34d2_vc_arm_1", df)
  w34d3_vc_comp = form_fields_complete(uniq_id, "w34d3_vc_arm_1", df)
  w34d4_vc_comp = form_fields_complete(uniq_id, "w34d4_vc_arm_1", df)
  # week 035 video chat
  w35d1_vc_date = date_fields_complete(uniq_id, "w35d1_vc_arm_1", df)
  w35d2_vc_date = date_fields_complete(uniq_id, "w35d2_vc_arm_1", df)
  w35d3_vc_date = date_fields_complete(uniq_id, "w35d3_vc_arm_1", df)
  w35d4_vc_date = date_fields_complete(uniq_id, "w35d4_vc_arm_1", df)
  w35d1_vc_comp = form_fields_complete(uniq_id, "w35d1_vc_arm_1", df)
  w35d2_vc_comp = form_fields_complete(uniq_id, "w35d2_vc_arm_1", df)
  w35d3_vc_comp = form_fields_complete(uniq_id, "w35d3_vc_arm_1", df)
  w35d4_vc_comp = form_fields_complete(uniq_id, "w35d4_vc_arm_1", df)
  # week 036 video chat
  w36d1_vc_date = date_fields_complete(uniq_id, "w36d1_vc_arm_1", df)
  w36d2_vc_date = date_fields_complete(uniq_id, "w36d2_vc_arm_1", df)
  w36d3_vc_date = date_fields_complete(uniq_id, "w36d3_vc_arm_1", df)
  w36d4_vc_date = date_fields_complete(uniq_id, "w36d4_vc_arm_1", df)
  w36d1_vc_comp = form_fields_complete(uniq_id, "w36d1_vc_arm_1", df)
  w36d2_vc_comp = form_fields_complete(uniq_id, "w36d2_vc_arm_1", df)
  w36d3_vc_comp = form_fields_complete(uniq_id, "w36d3_vc_arm_1", df)
  w36d4_vc_comp = form_fields_complete(uniq_id, "w36d4_vc_arm_1", df)
  # week 037 video chat
  w37d1_vc_date = date_fields_complete(uniq_id, "w37d1_vc_arm_1", df)
  w37d2_vc_date = date_fields_complete(uniq_id, "w37d2_vc_arm_1", df)
  w37d3_vc_date = date_fields_complete(uniq_id, "w37d3_vc_arm_1", df)
  w37d4_vc_date = date_fields_complete(uniq_id, "w37d4_vc_arm_1", df)
  w37d1_vc_comp = form_fields_complete(uniq_id, "w37d1_vc_arm_1", df)
  w37d2_vc_comp = form_fields_complete(uniq_id, "w37d2_vc_arm_1", df)
  w37d3_vc_comp = form_fields_complete(uniq_id, "w37d3_vc_arm_1", df)
  w37d4_vc_comp = form_fields_complete(uniq_id, "w37d4_vc_arm_1", df)
  # week 038 video chat
  w38d1_vc_date = date_fields_complete(uniq_id, "w38d1_vc_arm_1", df)
  w38d2_vc_date = date_fields_complete(uniq_id, "w38d2_vc_arm_1", df)
  w38d3_vc_date = date_fields_complete(uniq_id, "w38d3_vc_arm_1", df)
  w38d4_vc_date = date_fields_complete(uniq_id, "w38d4_vc_arm_1", df)
  w38d1_vc_comp = form_fields_complete(uniq_id, "w38d1_vc_arm_1", df)
  w38d2_vc_comp = form_fields_complete(uniq_id, "w38d2_vc_arm_1", df)
  w38d3_vc_comp = form_fields_complete(uniq_id, "w38d3_vc_arm_1", df)
  w38d4_vc_comp = form_fields_complete(uniq_id, "w38d4_vc_arm_1", df)
  # week 039 video chat
  w39d1_vc_date = date_fields_complete(uniq_id, "w39d1_vc_arm_1", df)
  w39d2_vc_date = date_fields_complete(uniq_id, "w39d2_vc_arm_1", df)
  w39d3_vc_date = date_fields_complete(uniq_id, "w39d3_vc_arm_1", df)
  w39d4_vc_date = date_fields_complete(uniq_id, "w39d4_vc_arm_1", df)
  w39d1_vc_comp = form_fields_complete(uniq_id, "w39d1_vc_arm_1", df)
  w39d2_vc_comp = form_fields_complete(uniq_id, "w39d2_vc_arm_1", df)
  w39d3_vc_comp = form_fields_complete(uniq_id, "w39d3_vc_arm_1", df)
  w39d4_vc_comp = form_fields_complete(uniq_id, "w39d4_vc_arm_1", df)
  # week 040 video chat
  w40d1_vc_date = date_fields_complete(uniq_id, "w40d1_vc_arm_1", df)
  w40d2_vc_date = date_fields_complete(uniq_id, "w40d2_vc_arm_1", df)
  w40d3_vc_date = date_fields_complete(uniq_id, "w40d3_vc_arm_1", df)
  w40d4_vc_date = date_fields_complete(uniq_id, "w40d4_vc_arm_1", df)
  w40d1_vc_comp = form_fields_complete(uniq_id, "w40d1_vc_arm_1", df)
  w40d2_vc_comp = form_fields_complete(uniq_id, "w40d2_vc_arm_1", df)
  w40d3_vc_comp = form_fields_complete(uniq_id, "w40d3_vc_arm_1", df)
  w40d4_vc_comp = form_fields_complete(uniq_id, "w40d4_vc_arm_1", df)
  # week 041 video chat
  w41d1_vc_date = date_fields_complete(uniq_id, "w41d1_vc_arm_1", df)
  w41d2_vc_date = date_fields_complete(uniq_id, "w41d2_vc_arm_1", df)
  w41d3_vc_date = date_fields_complete(uniq_id, "w41d3_vc_arm_1", df)
  w41d4_vc_date = date_fields_complete(uniq_id, "w41d4_vc_arm_1", df)
  w41d1_vc_comp = form_fields_complete(uniq_id, "w41d1_vc_arm_1", df)
  w41d2_vc_comp = form_fields_complete(uniq_id, "w41d2_vc_arm_1", df)
  w41d3_vc_comp = form_fields_complete(uniq_id, "w41d3_vc_arm_1", df)
  w41d4_vc_comp = form_fields_complete(uniq_id, "w41d4_vc_arm_1", df)
  # week 042 video chat
  w42d1_vc_date = date_fields_complete(uniq_id, "w42d1_vc_arm_1", df)
  w42d2_vc_date = date_fields_complete(uniq_id, "w42d2_vc_arm_1", df)
  w42d3_vc_date = date_fields_complete(uniq_id, "w42d3_vc_arm_1", df)
  w42d4_vc_date = date_fields_complete(uniq_id, "w42d4_vc_arm_1", df)
  w42d1_vc_comp = form_fields_complete(uniq_id, "w42d1_vc_arm_1", df)
  w42d2_vc_comp = form_fields_complete(uniq_id, "w42d2_vc_arm_1", df)
  w42d3_vc_comp = form_fields_complete(uniq_id, "w42d3_vc_arm_1", df)
  w42d4_vc_comp = form_fields_complete(uniq_id, "w42d4_vc_arm_1", df)
  # week 043 video chat
  w43d1_vc_date = date_fields_complete(uniq_id, "w43d1_vc_arm_1", df)
  w43d2_vc_date = date_fields_complete(uniq_id, "w43d2_vc_arm_1", df)
  w43d3_vc_date = date_fields_complete(uniq_id, "w43d3_vc_arm_1", df)
  w43d4_vc_date = date_fields_complete(uniq_id, "w43d4_vc_arm_1", df)
  w43d1_vc_comp = form_fields_complete(uniq_id, "w43d1_vc_arm_1", df)
  w43d2_vc_comp = form_fields_complete(uniq_id, "w43d2_vc_arm_1", df)
  w43d3_vc_comp = form_fields_complete(uniq_id, "w43d3_vc_arm_1", df)
  w43d4_vc_comp = form_fields_complete(uniq_id, "w43d4_vc_arm_1", df)
  # week 044 video chat
  w44d1_vc_date = date_fields_complete(uniq_id, "w44d1_vc_arm_1", df)
  w44d2_vc_date = date_fields_complete(uniq_id, "w44d2_vc_arm_1", df)
  w44d3_vc_date = date_fields_complete(uniq_id, "w44d3_vc_arm_1", df)
  w44d4_vc_date = date_fields_complete(uniq_id, "w44d4_vc_arm_1", df)
  w44d1_vc_comp = form_fields_complete(uniq_id, "w44d1_vc_arm_1", df)
  w44d2_vc_comp = form_fields_complete(uniq_id, "w44d2_vc_arm_1", df)
  w44d3_vc_comp = form_fields_complete(uniq_id, "w44d3_vc_arm_1", df)
  w44d4_vc_comp = form_fields_complete(uniq_id, "w44d4_vc_arm_1", df)
  # week 045 video chat
  w45d1_vc_date = date_fields_complete(uniq_id, "w45d1_vc_arm_1", df)
  w45d2_vc_date = date_fields_complete(uniq_id, "w45d2_vc_arm_1", df)
  w45d3_vc_date = date_fields_complete(uniq_id, "w45d3_vc_arm_1", df)
  w45d4_vc_date = date_fields_complete(uniq_id, "w45d4_vc_arm_1", df)
  w45d1_vc_comp = form_fields_complete(uniq_id, "w45d1_vc_arm_1", df)
  w45d2_vc_comp = form_fields_complete(uniq_id, "w45d2_vc_arm_1", df)
  w45d3_vc_comp = form_fields_complete(uniq_id, "w45d3_vc_arm_1", df)
  w45d4_vc_comp = form_fields_complete(uniq_id, "w45d4_vc_arm_1", df)
  # week 046 video chat
  w46d1_vc_date = date_fields_complete(uniq_id, "w46d1_vc_arm_1", df)
  w46d2_vc_date = date_fields_complete(uniq_id, "w46d2_vc_arm_1", df)
  w46d3_vc_date = date_fields_complete(uniq_id, "w46d3_vc_arm_1", df)
  w46d4_vc_date = date_fields_complete(uniq_id, "w46d4_vc_arm_1", df)
  w46d1_vc_comp = form_fields_complete(uniq_id, "w46d1_vc_arm_1", df)
  w46d2_vc_comp = form_fields_complete(uniq_id, "w46d2_vc_arm_1", df)
  w46d3_vc_comp = form_fields_complete(uniq_id, "w46d3_vc_arm_1", df)
  w46d4_vc_comp = form_fields_complete(uniq_id, "w46d4_vc_arm_1", df)
  # week 047 video chat
  w47d1_vc_date = date_fields_complete(uniq_id, "w47d1_vc_arm_1", df)
  w47d2_vc_date = date_fields_complete(uniq_id, "w47d2_vc_arm_1", df)
  w47d3_vc_date = date_fields_complete(uniq_id, "w47d3_vc_arm_1", df)
  w47d4_vc_date = date_fields_complete(uniq_id, "w47d4_vc_arm_1", df)
  w47d1_vc_comp = form_fields_complete(uniq_id, "w47d1_vc_arm_1", df)
  w47d2_vc_comp = form_fields_complete(uniq_id, "w47d2_vc_arm_1", df)
  w47d3_vc_comp = form_fields_complete(uniq_id, "w47d3_vc_arm_1", df)
  w47d4_vc_comp = form_fields_complete(uniq_id, "w47d4_vc_arm_1", df)
  # week 048 video chat
  w48d1_vc_date = date_fields_complete(uniq_id, "w48d1_vc_arm_1", df)
  w48d2_vc_date = date_fields_complete(uniq_id, "w48d2_vc_arm_1", df)
  w48d3_vc_date = date_fields_complete(uniq_id, "w48d3_vc_arm_1", df)
  w48d4_vc_date = date_fields_complete(uniq_id, "w48d4_vc_arm_1", df)
  w48d1_vc_comp = form_fields_complete(uniq_id, "w48d1_vc_arm_1", df)
  w48d2_vc_comp = form_fields_complete(uniq_id, "w48d2_vc_arm_1", df)
  w48d3_vc_comp = form_fields_complete(uniq_id, "w48d3_vc_arm_1", df)
  w48d4_vc_comp = form_fields_complete(uniq_id, "w48d4_vc_arm_1", df)
  
  tibble(ts_sub_id,
         # # name
         # ts_lfn, ts_pfn, ts_lln,
         # screen telephone
         scrn_tel_date, scrn_tel_comp,
         # screen visit
         scrn_v_date,   scrn_v_comp,
         # baseline visit 1
         bv1_date,      bv1_comp,
         # baseline clinician dx
         bl_cdx_date,   bl_cdx_comp,
         # baseline visit 2
         bv2_date,      bv2_comp,
         # randomization
         admin_date,    admin_comp,
         # baseline mri
         bl_mri_date,   bl_mri_comp,
         # weekly telephone
         w01_tel_date,  w01_tel_comp,
         w02_tel_date,  w02_tel_comp,
         w03_tel_date,  w03_tel_comp,
         w04_tel_date,  w04_tel_comp,
         w05_tel_date,  w05_tel_comp,
         w06_tel_date,  w06_tel_comp,
         w07_tel_date,  w07_tel_comp,
         w08_tel_date,  w08_tel_comp,
         w09_tel_date,  w09_tel_comp,
         w10_tel_date,  w10_tel_comp,
         w11_tel_date,  w11_tel_comp,
         w12_tel_date,  w12_tel_comp,
         w13_tel_date,  w13_tel_comp,
         w14_tel_date,  w14_tel_comp,
         w15_tel_date,  w15_tel_comp,
         w16_tel_date,  w16_tel_comp,
         w17_tel_date,  w17_tel_comp,
         w18_tel_date,  w18_tel_comp,
         w19_tel_date,  w19_tel_comp,
         w20_tel_date,  w20_tel_comp,
         w21_tel_date,  w21_tel_comp,
         w22_tel_date,  w22_tel_comp,
         w23_tel_date,  w23_tel_comp,
         w24_tel_date,  w24_tel_comp,
         w25_tel_date,  w25_tel_comp,
         w26_tel_date,  w26_tel_comp,
         w27_tel_date,  w27_tel_comp,
         w28_tel_date,  w28_tel_comp,
         w29_tel_date,  w29_tel_comp,
         w30_tel_date,  w30_tel_comp,
         w31_tel_date,  w31_tel_comp,
         w32_tel_date,  w32_tel_comp,
         w33_tel_date,  w33_tel_comp,
         w34_tel_date,  w34_tel_comp,
         w35_tel_date,  w35_tel_comp,
         w36_tel_date,  w36_tel_comp,
         w37_tel_date,  w37_tel_comp,
         w38_tel_date,  w38_tel_comp,
         w39_tel_date,  w39_tel_comp,
         w40_tel_date,  w40_tel_comp,
         w41_tel_date,  w41_tel_comp,
         w42_tel_date,  w42_tel_comp,
         w43_tel_date,  w43_tel_comp,
         w44_tel_date,  w44_tel_comp,
         w45_tel_date,  w45_tel_comp,
         w46_tel_date,  w46_tel_comp,
         w47_tel_date,  w47_tel_comp,
         w48_tel_date,  w48_tel_comp,
         # daily video chat
         w01d1_vc_date, w01d2_vc_date, w01d3_vc_date, w01d4_vc_date,
         w01d1_vc_comp, w01d2_vc_comp, w01d3_vc_comp, w01d4_vc_comp,
         w02d1_vc_date, w02d2_vc_date, w02d3_vc_date, w02d4_vc_date,
         w02d1_vc_comp, w02d2_vc_comp, w02d3_vc_comp, w02d4_vc_comp,
         w03d1_vc_date, w03d2_vc_date, w03d3_vc_date, w03d4_vc_date,
         w03d1_vc_comp, w03d2_vc_comp, w03d3_vc_comp, w03d4_vc_comp,
         w04d1_vc_date, w04d2_vc_date, w04d3_vc_date, w04d4_vc_date,
         w04d1_vc_comp, w04d2_vc_comp, w04d3_vc_comp, w04d4_vc_comp,
         w05d1_vc_date, w05d2_vc_date, w05d3_vc_date, w05d4_vc_date,
         w05d1_vc_comp, w05d2_vc_comp, w05d3_vc_comp, w05d4_vc_comp,
         w06d1_vc_date, w06d2_vc_date, w06d3_vc_date, w06d4_vc_date,
         w06d1_vc_comp, w06d2_vc_comp, w06d3_vc_comp, w06d4_vc_comp,
         w07d1_vc_date, w07d2_vc_date, w07d3_vc_date, w07d4_vc_date,
         w07d1_vc_comp, w07d2_vc_comp, w07d3_vc_comp, w07d4_vc_comp,
         w08d1_vc_date, w08d2_vc_date, w08d3_vc_date, w08d4_vc_date,
         w08d1_vc_comp, w08d2_vc_comp, w08d3_vc_comp, w08d4_vc_comp,
         w09d1_vc_date, w09d2_vc_date, w09d3_vc_date, w09d4_vc_date,
         w09d1_vc_comp, w09d2_vc_comp, w09d3_vc_comp, w09d4_vc_comp,
         w10d1_vc_date, w10d2_vc_date, w10d3_vc_date, w10d4_vc_date,
         w10d1_vc_comp, w10d2_vc_comp, w10d3_vc_comp, w10d4_vc_comp,
         w11d1_vc_date, w11d2_vc_date, w11d3_vc_date, w11d4_vc_date,
         w11d1_vc_comp, w11d2_vc_comp, w11d3_vc_comp, w11d4_vc_comp,
         w12d1_vc_date, w12d2_vc_date, w12d3_vc_date, w12d4_vc_date,
         w12d1_vc_comp, w12d2_vc_comp, w12d3_vc_comp, w12d4_vc_comp,
         w13d1_vc_date, w13d2_vc_date, w13d3_vc_date, w13d4_vc_date,
         w13d1_vc_comp, w13d2_vc_comp, w13d3_vc_comp, w13d4_vc_comp,
         w14d1_vc_date, w14d2_vc_date, w14d3_vc_date, w14d4_vc_date,
         w14d1_vc_comp, w14d2_vc_comp, w14d3_vc_comp, w14d4_vc_comp,
         w15d1_vc_date, w15d2_vc_date, w15d3_vc_date, w15d4_vc_date,
         w15d1_vc_comp, w15d2_vc_comp, w15d3_vc_comp, w15d4_vc_comp,
         w16d1_vc_date, w16d2_vc_date, w16d3_vc_date, w16d4_vc_date,
         w16d1_vc_comp, w16d2_vc_comp, w16d3_vc_comp, w16d4_vc_comp,
         w17d1_vc_date, w17d2_vc_date, w17d3_vc_date, w17d4_vc_date,
         w17d1_vc_comp, w17d2_vc_comp, w17d3_vc_comp, w17d4_vc_comp,
         w18d1_vc_date, w18d2_vc_date, w18d3_vc_date, w18d4_vc_date,
         w18d1_vc_comp, w18d2_vc_comp, w18d3_vc_comp, w18d4_vc_comp,
         w19d1_vc_date, w19d2_vc_date, w19d3_vc_date, w19d4_vc_date,
         w19d1_vc_comp, w19d2_vc_comp, w19d3_vc_comp, w19d4_vc_comp,
         w20d1_vc_date, w20d2_vc_date, w20d3_vc_date, w20d4_vc_date,
         w20d1_vc_comp, w20d2_vc_comp, w20d3_vc_comp, w20d4_vc_comp,
         w21d1_vc_date, w21d2_vc_date, w21d3_vc_date, w21d4_vc_date,
         w21d1_vc_comp, w21d2_vc_comp, w21d3_vc_comp, w21d4_vc_comp,
         w22d1_vc_date, w22d2_vc_date, w22d3_vc_date, w22d4_vc_date,
         w22d1_vc_comp, w22d2_vc_comp, w22d3_vc_comp, w22d4_vc_comp,
         w23d1_vc_date, w23d2_vc_date, w23d3_vc_date, w23d4_vc_date,
         w23d1_vc_comp, w23d2_vc_comp, w23d3_vc_comp, w23d4_vc_comp,
         w24d1_vc_date, w24d2_vc_date, w24d3_vc_date, w24d4_vc_date,
         w24d1_vc_comp, w24d2_vc_comp, w24d3_vc_comp, w24d4_vc_comp,
         w25d1_vc_date, w25d2_vc_date, w25d3_vc_date, w25d4_vc_date,
         w25d1_vc_comp, w25d2_vc_comp, w25d3_vc_comp, w25d4_vc_comp,
         w26d1_vc_date, w26d2_vc_date, w26d3_vc_date, w26d4_vc_date,
         w26d1_vc_comp, w26d2_vc_comp, w26d3_vc_comp, w26d4_vc_comp,
         w27d1_vc_date, w27d2_vc_date, w27d3_vc_date, w27d4_vc_date,
         w27d1_vc_comp, w27d2_vc_comp, w27d3_vc_comp, w27d4_vc_comp,
         w28d1_vc_date, w28d2_vc_date, w28d3_vc_date, w28d4_vc_date,
         w28d1_vc_comp, w28d2_vc_comp, w28d3_vc_comp, w28d4_vc_comp,
         w29d1_vc_date, w29d2_vc_date, w29d3_vc_date, w29d4_vc_date,
         w29d1_vc_comp, w29d2_vc_comp, w29d3_vc_comp, w29d4_vc_comp,
         w30d1_vc_date, w30d2_vc_date, w30d3_vc_date, w30d4_vc_date,
         w30d1_vc_comp, w30d2_vc_comp, w30d3_vc_comp, w30d4_vc_comp,
         w31d1_vc_date, w31d2_vc_date, w31d3_vc_date, w31d4_vc_date,
         w31d1_vc_comp, w31d2_vc_comp, w31d3_vc_comp, w31d4_vc_comp,
         w32d1_vc_date, w32d2_vc_date, w32d3_vc_date, w32d4_vc_date,
         w32d1_vc_comp, w32d2_vc_comp, w32d3_vc_comp, w32d4_vc_comp,
         w33d1_vc_date, w33d2_vc_date, w33d3_vc_date, w33d4_vc_date,
         w33d1_vc_comp, w33d2_vc_comp, w33d3_vc_comp, w33d4_vc_comp,
         w34d1_vc_date, w34d2_vc_date, w34d3_vc_date, w34d4_vc_date,
         w34d1_vc_comp, w34d2_vc_comp, w34d3_vc_comp, w34d4_vc_comp,
         w35d1_vc_date, w35d2_vc_date, w35d3_vc_date, w35d4_vc_date,
         w35d1_vc_comp, w35d2_vc_comp, w35d3_vc_comp, w35d4_vc_comp,
         w36d1_vc_date, w36d2_vc_date, w36d3_vc_date, w36d4_vc_date,
         w36d1_vc_comp, w36d2_vc_comp, w36d3_vc_comp, w36d4_vc_comp,
         w37d1_vc_date, w37d2_vc_date, w37d3_vc_date, w37d4_vc_date,
         w37d1_vc_comp, w37d2_vc_comp, w37d3_vc_comp, w37d4_vc_comp,
         w38d1_vc_date, w38d2_vc_date, w38d3_vc_date, w38d4_vc_date,
         w38d1_vc_comp, w38d2_vc_comp, w38d3_vc_comp, w38d4_vc_comp,
         w39d1_vc_date, w39d2_vc_date, w39d3_vc_date, w39d4_vc_date,
         w39d1_vc_comp, w39d2_vc_comp, w39d3_vc_comp, w39d4_vc_comp,
         w40d1_vc_date, w40d2_vc_date, w40d3_vc_date, w40d4_vc_date,
         w40d1_vc_comp, w40d2_vc_comp, w40d3_vc_comp, w40d4_vc_comp,
         w41d1_vc_date, w41d2_vc_date, w41d3_vc_date, w41d4_vc_date,
         w41d1_vc_comp, w41d2_vc_comp, w41d3_vc_comp, w41d4_vc_comp,
         w42d1_vc_date, w42d2_vc_date, w42d3_vc_date, w42d4_vc_date,
         w42d1_vc_comp, w42d2_vc_comp, w42d3_vc_comp, w42d4_vc_comp,
         w43d1_vc_date, w43d2_vc_date, w43d3_vc_date, w43d4_vc_date,
         w43d1_vc_comp, w43d2_vc_comp, w43d3_vc_comp, w43d4_vc_comp,
         w44d1_vc_date, w44d2_vc_date, w44d3_vc_date, w44d4_vc_date,
         w44d1_vc_comp, w44d2_vc_comp, w44d3_vc_comp, w44d4_vc_comp,
         w45d1_vc_date, w45d2_vc_date, w45d3_vc_date, w45d4_vc_date,
         w45d1_vc_comp, w45d2_vc_comp, w45d3_vc_comp, w45d4_vc_comp,
         w46d1_vc_date, w46d2_vc_date, w46d3_vc_date, w46d4_vc_date,
         w46d1_vc_comp, w46d2_vc_comp, w46d3_vc_comp, w46d4_vc_comp,
         w47d1_vc_date, w47d2_vc_date, w47d3_vc_date, w47d4_vc_date,
         w47d1_vc_comp, w47d2_vc_comp, w47d3_vc_comp, w47d4_vc_comp,
         w48d1_vc_date, w48d2_vc_date, w48d3_vc_date, w48d4_vc_date,
         w48d1_vc_comp, w48d2_vc_comp, w48d3_vc_comp, w48d4_vc_comp
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

# uniq_ids
# build_distilled_row("C2006")
# build_distilled_row("C2017")

data_dstl <- purrr::map_df(uniq_ids, build_distilled_row, data_slct_fltr_cln)

# fwrite(data_dstl, "data_dstl.csv", na = "")

data_summ <- data_dstl %>% 
  transmute(
    ts_sub_id,
    # ts_lfn,
    # ts_pfn,
    # ts_lln,
    stage_comp = case_when(
    # weekly telephone
    w48_tel_date  & w48_tel_comp  ~ "w48_tel",
    w47_tel_date  & w47_tel_comp  ~ "w47_tel",
    w46_tel_date  & w46_tel_comp  ~ "w46_tel",
    w45_tel_date  & w45_tel_comp  ~ "w45_tel",
    w44_tel_date  & w44_tel_comp  ~ "w44_tel",
    w43_tel_date  & w43_tel_comp  ~ "w43_tel",
    w42_tel_date  & w42_tel_comp  ~ "w42_tel",
    w41_tel_date  & w41_tel_comp  ~ "w41_tel",
    w40_tel_date  & w40_tel_comp  ~ "w40_tel",
    w39_tel_date  & w39_tel_comp  ~ "w39_tel",
    w38_tel_date  & w38_tel_comp  ~ "w38_tel",
    w37_tel_date  & w37_tel_comp  ~ "w37_tel",
    w36_tel_date  & w36_tel_comp  ~ "w36_tel",
    w35_tel_date  & w35_tel_comp  ~ "w35_tel",
    w34_tel_date  & w34_tel_comp  ~ "w34_tel",
    w33_tel_date  & w33_tel_comp  ~ "w33_tel",
    w32_tel_date  & w32_tel_comp  ~ "w32_tel",
    w31_tel_date  & w31_tel_comp  ~ "w31_tel",
    w30_tel_date  & w30_tel_comp  ~ "w30_tel",
    w29_tel_date  & w29_tel_comp  ~ "w29_tel",
    w28_tel_date  & w28_tel_comp  ~ "w28_tel",
    w27_tel_date  & w27_tel_comp  ~ "w27_tel",
    w26_tel_date  & w26_tel_comp  ~ "w26_tel",
    w25_tel_date  & w25_tel_comp  ~ "w25_tel",
    w24_tel_date  & w24_tel_comp  ~ "w24_tel",
    w23_tel_date  & w23_tel_comp  ~ "w23_tel",
    w22_tel_date  & w22_tel_comp  ~ "w22_tel",
    w21_tel_date  & w21_tel_comp  ~ "w21_tel",
    w20_tel_date  & w20_tel_comp  ~ "w20_tel",
    w19_tel_date  & w19_tel_comp  ~ "w19_tel",
    w18_tel_date  & w18_tel_comp  ~ "w18_tel",
    w17_tel_date  & w17_tel_comp  ~ "w17_tel",
    w16_tel_date  & w16_tel_comp  ~ "w16_tel",
    w15_tel_date  & w15_tel_comp  ~ "w15_tel",
    w14_tel_date  & w14_tel_comp  ~ "w14_tel",
    w13_tel_date  & w13_tel_comp  ~ "w13_tel",
    w12_tel_date  & w12_tel_comp  ~ "w12_tel",
    w11_tel_date  & w11_tel_comp  ~ "w11_tel",
    w10_tel_date  & w10_tel_comp  ~ "w10_tel",
    w09_tel_date  & w09_tel_comp  ~ "w09_tel",
    w08_tel_date  & w08_tel_comp  ~ "w08_tel",
    w07_tel_date  & w07_tel_comp  ~ "w07_tel",
    w06_tel_date  & w06_tel_comp  ~ "w06_tel",
    w05_tel_date  & w05_tel_comp  ~ "w05_tel",
    w04_tel_date  & w04_tel_comp  ~ "w04_tel",
    w03_tel_date  & w03_tel_comp  ~ "w03_tel",
    w02_tel_date  & w02_tel_comp  ~ "w02_tel",
    w01_tel_date  & w01_tel_comp  ~ "w01_tel",
    # baseline MRI
    bl_mri_date   & bl_mri_comp   ~ "bl_mri",
    # randomization
    admin_comp    ~ "admin",   # admin_date is NA
    # baseline visit 2
    bv2_date      & bv2_comp      ~ "bv2",
    # baseline clinician dx
    bl_cdx_date   & bl_cdx_comp   ~ "bl_cdx",
    # baseline visit 1
    bv1_date      & bv1_comp      ~ "bv1",
    # screening visit
    scrn_v_date   & scrn_v_comp   ~ "scrn_v",
    # telephone screening
    scrn_tel_date & scrn_tel_comp ~ "scrn_tel",
    # catch-all
    TRUE ~ NA_character_
  )) %>% 
  mutate(
    stage_next = case_when(
      is.na(stage_comp)        ~ "scrn_tel",
      stage_comp == "scrn_tel" ~ "scrn_v",
      stage_comp == "scrn_v"   ~ "bv1",
      stage_comp == "bv1"      ~ "bl_cdx",
      stage_comp == "bl_cdx"   ~ "bv2",
      stage_comp == "bv2"      ~ "admin",
      stage_comp == "admin"    ~ "bl_mri",
      stage_comp == "bl_mri"   ~ "w01_tel",
      stage_comp == "w01_tel"  ~ "w02_tel",
      stage_comp == "w02_tel"  ~ "w03_tel",
      stage_comp == "w03_tel"  ~ "w04_tel",
      stage_comp == "w04_tel"  ~ "w05_tel",
      stage_comp == "w05_tel"  ~ "w06_tel",
      stage_comp == "w06_tel"  ~ "w07_tel",
      stage_comp == "w07_tel"  ~ "w08_tel",
      stage_comp == "w08_tel"  ~ "w09_tel",
      stage_comp == "w09_tel"  ~ "w10_tel",
      stage_comp == "w10_tel"  ~ "w11_tel",
      stage_comp == "w11_tel"  ~ "w12_tel",
      stage_comp == "w12_tel"  ~ "w13_tel",
      stage_comp == "w13_tel"  ~ "w14_tel",
      stage_comp == "w14_tel"  ~ "w15_tel",
      stage_comp == "w15_tel"  ~ "w16_tel",
      stage_comp == "w16_tel"  ~ "w17_tel",
      stage_comp == "w17_tel"  ~ "w18_tel",
      stage_comp == "w18_tel"  ~ "w19_tel",
      stage_comp == "w19_tel"  ~ "w20_tel",
      stage_comp == "w20_tel"  ~ "w21_tel",
      stage_comp == "w21_tel"  ~ "w22_tel",
      stage_comp == "w22_tel"  ~ "w23_tel",
      stage_comp == "w23_tel"  ~ "w24_tel",
      stage_comp == "w24_tel"  ~ "w25_tel",
      stage_comp == "w25_tel"  ~ "w26_tel",
      stage_comp == "w26_tel"  ~ "w27_tel",
      stage_comp == "w27_tel"  ~ "w28_tel",
      stage_comp == "w28_tel"  ~ "w29_tel",
      stage_comp == "w29_tel"  ~ "w30_tel",
      stage_comp == "w30_tel"  ~ "w31_tel",
      stage_comp == "w31_tel"  ~ "w32_tel",
      stage_comp == "w32_tel"  ~ "w33_tel",
      stage_comp == "w33_tel"  ~ "w34_tel",
      stage_comp == "w34_tel"  ~ "w35_tel",
      stage_comp == "w35_tel"  ~ "w36_tel",
      stage_comp == "w36_tel"  ~ "w37_tel",
      stage_comp == "w37_tel"  ~ "w38_tel",
      stage_comp == "w38_tel"  ~ "w39_tel",
      stage_comp == "w39_tel"  ~ "w40_tel",
      stage_comp == "w40_tel"  ~ "w41_tel",
      stage_comp == "w41_tel"  ~ "w42_tel",
      stage_comp == "w42_tel"  ~ "w43_tel",
      stage_comp == "w43_tel"  ~ "w44_tel",
      stage_comp == "w44_tel"  ~ "w45_tel",
      stage_comp == "w45_tel"  ~ "w46_tel",
      stage_comp == "w46_tel"  ~ "w47_tel",
      stage_comp == "w47_tel"  ~ "w48_tel",
      stage_comp == "w48_tel"  ~ "finished"
    )
  )

saveRDS(data_summ, "./rds/data_summ.Rds")


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
