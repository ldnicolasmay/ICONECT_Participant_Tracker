library(dplyr)

weeks_1 <- 1:26
weeks_2 <- 27:52
df <- tibble(
  step = vector(mode = "character", 
                length = length(c(weeks_1, weeks_2)) * 5),
  stage = vector(mode = "character", 
                 length = length(c(weeks_1, weeks_2)) * 5),
  rc_field = vector(mode = "character", 
                    length = length(c(weeks_1, weeks_2)) * 5))


for (i in weeks_1) {
  df[(i-1)*5+1, ] <- 
    list(paste0("Telephone Chat - Week ", i), 
         "Intervention Period 1", NA_character_)
  df[(i-1)*5+2, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 1"), 
         "Intervention Period 1", NA_character_)
  df[(i-1)*5+3, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 2"), 
         "Intervention Period 1", NA_character_)
  df[(i-1)*5+4, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 3"), 
         "Intervention Period 1", NA_character_)
  df[(i-1)*5+5, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 4"), 
         "Intervention Period 1", NA_character_)
}

for (i in weeks_2) {
  df[(i-1)*5+1, ] <- 
    list(paste0("Telephone Chat - Week ", i), 
         "Intervention Period 2", NA_character_)
  df[(i-1)*5+2, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 1"), 
         "Intervention Period 2", NA_character_)
  df[(i-1)*5+3, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 2"), 
         "Intervention Period 2", NA_character_)
  df[(i-1)*5+4, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 3"), 
         "Intervention Period 2", NA_character_)
  df[(i-1)*5+5, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 4"), 
         "Intervention Period 2", NA_character_)
}

readr::write_csv(df, "df.csv", na = "")

# ++++++++++++++++++++++++++++ #

# # weekly phone calls
# w01_tel_arm_1  = c("wkq_dat"),

for (i in 1:48) {
  if (nchar(i) == 1) cat(paste0("w0", i, "_tel_arm_1  = c(\"wkq_dat\"),\n" ))
  else cat(paste0("w", i, "_tel_arm_1  = c(\"wkq_dat\"),\n" ))
}

# # weekly phone calls
# w01_tel_arm_1  = c("iconect_weekly_questionnaire_complete"),

for (i in 1:48) {
  if (nchar(i) == 1) {
    cat(paste0("w0", i, "_tel_arm_1  = c(\"iconect_weekly_questionnaire_complete\"),\n" ))
  } else {
    cat(paste0("w", i, "_tel_arm_1  = c(\"iconect_weekly_questionnaire_complete\"),\n" ))
  }
}

# ++++++++++++++++++++++++++++ #

# # daily video chats
# w01d1_vc_arm_1 = c("vcd_dat"),
# w01d2_vc_arm_1 = c("vcd_dat"),
# w01d3_vc_arm_1 = c("vcd_dat"),
# w01d4_vc_arm_1 = c("vcd_dat"),

for (i in 1:48) {
  if (nchar(i) == 1) {
    cat(paste0(
      "w0", i, "d1_vc_arm_1 = c(\"vcd_dat\"),\n",
      "w0", i, "d2_vc_arm_1 = c(\"vcd_dat\"),\n",
      "w0", i, "d3_vc_arm_1 = c(\"vcd_dat\"),\n",
      "w0", i, "d4_vc_arm_1 = c(\"vcd_dat\"),\n"
    ))
  } else {
    cat(paste0(
      "w", i, "d1_vc_arm_1 = c(\"vcd_dat\"),\n",
      "w", i, "d2_vc_arm_1 = c(\"vcd_dat\"),\n",
      "w", i, "d3_vc_arm_1 = c(\"vcd_dat\"),\n",
      "w", i, "d4_vc_arm_1 = c(\"vcd_dat\"),\n"
    ))
  }
}

# # daily video chats
# w01d1_vc_arm_1 = c("video_chat_daily_form_complete"),
# w01d2_vc_arm_1 = c("video_chat_daily_form_complete"),
# w01d3_vc_arm_1 = c("video_chat_daily_form_complete"),
# w01d4_vc_arm_1 = c("video_chat_daily_form_complete"),

for (i in 1:48) {
  if (nchar(i) == 1) {
    cat(paste0(
      "w0", i, "d1_vc_arm_1 = c(\"video_chat_daily_form_complete\"),\n",
      "w0", i, "d2_vc_arm_1 = c(\"video_chat_daily_form_complete\"),\n",
      "w0", i, "d3_vc_arm_1 = c(\"video_chat_daily_form_complete\"),\n",
      "w0", i, "d4_vc_arm_1 = c(\"video_chat_daily_form_complete\"),\n"
    ))
  } else {
    cat(paste0(
      "w", i, "d1_vc_arm_1 = c(\"video_chat_daily_form_complete\"),\n",
      "w", i, "d2_vc_arm_1 = c(\"video_chat_daily_form_complete\"),\n",
      "w", i, "d3_vc_arm_1 = c(\"video_chat_daily_form_complete\"),\n",
      "w", i, "d4_vc_arm_1 = c(\"video_chat_daily_form_complete\"),\n"
    ))
  }
}

# ++++++++++++++++++++++++++++ #

# # week 01 tel
# w01_tel_date =
#   date_fields_complete(uniq_id, "w01_tel_arm_1", df)
# w01_tel_comp =
#   form_fields_complete(uniq_id, "w01_tel_arm_1", df)

for (i in 1:48) {
  if (nchar(i) == 1) {
    cat(paste0(
      "# week 0", i, " tel\n",
      "w0", i, "_tel_date = date_fields_complete(uniq_id, \"w0", i, "_tel_arm_1\", df)\n",
      "w0", i, "_tel_comp = form_fields_complete(uniq_id, \"w0", i, "_tel_arm_1\", df)\n"
    ))
  } else {
    cat(paste0(
      "# week ", i, " tel\n",
      "w", i, "_tel_date = date_fields_complete(uniq_id, \"w", i, "_tel_arm_1\", df)\n",
      "w", i, "_tel_comp = form_fields_complete(uniq_id, \"w", i, "_tel_arm_1\", df)\n"
    ))
  }
}

# ++++++++++++++++++++++++++++ #

# # week 01 video chat
# w01d1_vc_date =
#   date_fields_complete(uniq_id, "w01d1_vc_arm_1", df)
# w01d2_vc_date =
#   date_fields_complete(uniq_id, "w01d2_vc_arm_1", df)
# w01d3_vc_date =
#   date_fields_complete(uniq_id, "w01d3_vc_arm_1", df)
# w01d4_vc_date =
#   date_fields_complete(uniq_id, "w01d4_vc_arm_1", df)
# w01d1_vc_comp =
#   form_fields_complete(uniq_id, "w01d1_vc_arm_1", df)
# w01d2_vc_comp =
#   form_fields_complete(uniq_id, "w01d2_vc_arm_1", df)
# w01d3_vc_comp =
#   form_fields_complete(uniq_id, "w01d3_vc_arm_1", df)
# w01d4_vc_comp =
#   form_fields_complete(uniq_id, "w01d4_vc_arm_1", df)

for (i in 1:48) {
  if (nchar(i) == 1) {
    cat(paste0(
      "# week 0", i, " video chat\n",
      "w0", i, "d1_vc_date = date_fields_complete(uniq_id, \"w0", i, "d1_vc_arm_1\", df)\n",
      "w0", i, "d2_vc_date = date_fields_complete(uniq_id, \"w0", i, "d2_vc_arm_1\", df)\n",
      "w0", i, "d3_vc_date = date_fields_complete(uniq_id, \"w0", i, "d3_vc_arm_1\", df)\n",
      "w0", i, "d4_vc_date = date_fields_complete(uniq_id, \"w0", i, "d4_vc_arm_1\", df)\n",
      "w0", i, "d1_vc_comp = form_fields_complete(uniq_id, \"w0", i, "d1_vc_arm_1\", df)\n",
      "w0", i, "d2_vc_comp = form_fields_complete(uniq_id, \"w0", i, "d2_vc_arm_1\", df)\n",
      "w0", i, "d3_vc_comp = form_fields_complete(uniq_id, \"w0", i, "d3_vc_arm_1\", df)\n",
      "w0", i, "d4_vc_comp = form_fields_complete(uniq_id, \"w0", i, "d4_vc_arm_1\", df)\n"
    ))
  } else {
    cat(paste0(
      "# week 0", i, " video chat\n",
      "w", i, "d1_vc_date = date_fields_complete(uniq_id, \"w", i, "d1_vc_arm_1\", df)\n",
      "w", i, "d2_vc_date = date_fields_complete(uniq_id, \"w", i, "d2_vc_arm_1\", df)\n",
      "w", i, "d3_vc_date = date_fields_complete(uniq_id, \"w", i, "d3_vc_arm_1\", df)\n",
      "w", i, "d4_vc_date = date_fields_complete(uniq_id, \"w", i, "d4_vc_arm_1\", df)\n",
      "w", i, "d1_vc_comp = form_fields_complete(uniq_id, \"w", i, "d1_vc_arm_1\", df)\n",
      "w", i, "d2_vc_comp = form_fields_complete(uniq_id, \"w", i, "d2_vc_arm_1\", df)\n",
      "w", i, "d3_vc_comp = form_fields_complete(uniq_id, \"w", i, "d3_vc_arm_1\", df)\n",
      "w", i, "d4_vc_comp = form_fields_complete(uniq_id, \"w", i, "d4_vc_arm_1\", df)\n"
    ))
  }
}

# ++++++++++++++++++++++++++++ #

# w01_tel_date,  w01_tel_comp,

for (i in 1:48) {
  if (nchar(i) == 1) {
    cat(paste0(
      "w0", i, "_tel_date,  w0", i, "_tel_comp,\n"
    ))
  } else {
    cat(paste0(
      "w", i, "_tel_date,  w", i, "_tel_comp,\n"
    ))
  }
}

# ++++++++++++++++++++++++++++ #

# w01d1_vc_date, w01d2_vc_date, w01d3_vc_date, w01d4_vc_date,
# w01d1_vc_comp, w01d2_vc_comp, w01d3_vc_comp, w01d4_vc_comp

for (i in 1:48) {
  if (nchar(i) == 1) {
    cat(paste0(
      "w0", i, "d1_vc_date, w0", i, "d2_vc_date, w0", i, "d3_vc_date, w0", i, "d4_vc_date,\n",
      "w0", i, "d1_vc_comp, w0", i, "d2_vc_comp, w0", i, "d3_vc_comp, w0", i, "d4_vc_comp,\n"
    ))
  } else {
    cat(paste0(
      "w", i, "d1_vc_date, w", i, "d2_vc_date, w", i, "d3_vc_date, w", i, "d4_vc_date,\n",
      "w", i, "d1_vc_comp, w", i, "d2_vc_comp, w", i, "d3_vc_comp, w", i, "d4_vc_comp,\n"
    ))
  }
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



