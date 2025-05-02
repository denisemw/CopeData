url <- "https://openredcap.nyumc.org/apps/redcap/api/"

#' Load a Matrix
#'
#' This function will download any requested survey from REDCap.
#' The following missing data codes in REDCap are replaced
#' with NA: -888,888, 8888, -999, 999, 9999. Only complete
#' surveys are returned.
#'
#'
#' @param token Unique REDCap token ID
#' @param form Name of the survey to be downloaded
#' @param raw_v_label Whether raw data or labels are requested
#' @param form_complete Whether complete or all surveys are downloaded (default is T)
#' @return A data frame for the completed surveys
#' @export
get_data <- function(token = token, form = form, raw_v_label = 'raw', form_complete = T) {
  if (form_complete) {
    record_filter = paste("[", form, "_complete]=2", sep = "")
  } else {
    record_filter = ""
  }
  formData <- list(uri = url,
                   "token"=token,
                   content='record',
                   format='csv',
                   type='flat',
                   csvDelimiter='',
                   'fields[0]'='record_id',
                   'forms[0]'=form,
                   rawOrLabel=raw_v_label,
                   rawOrLabelHeaders=raw_v_label,
                   exportCheckboxLabel='false',
                   exportSurveyFields='true',
                   exportDataAccessGroups='false',
                   returnFormat='csv',
                   filterLogic=record_filter)

  response <- httr::POST(url, body = formData, encode = "form")
  df <- httr::content(response)
  df[df == -888] = NA
  df[df == 888] = NA
  df[df == 8888] = NA
  df[df == -999] = NA
  df[df == 999] = NA
  df[df == 9999] = NA
  df[df == 9999999] = NA
  df[df == 999999] = NA
  df[df == 99999] = NA
  return (df)
}

#' Find prenatal COVID+ moms
#'
#' This function will find infant record IDs of moms who self-reported having covid during pregnancy
#'
#' @param token Unique REDCap token ID
#' @returns A data frame with record IDs of moms self-reporting as having covid during pregnancy
#' @export
get_prenatal_covid_records <- function(token) {
  ## get record IDs from the Covid 19 Survey For Pregnant Women
  preg_baseline = get_data(token, "covid19_survey_for_pregnant_women")
  preg_baseline = preg_baseline[,c("record_id")]

  ## pull the Covid 19 Survey All survey
  baseline = get_data(token, form = "covid_19_survey_all", "raw")

  ## keep subjects who reported having a positive covid test or prior/current symptoms
  baseline = baseline %>% filter(self_test==2 | self_symp == 2 | self_symp == 3)
  baseline = baseline[, c("record_id", "self_test", "self_symp")]

  ## keep subjects who were pregnant at the time
  baseline = merge(baseline, preg_baseline, by="record_id")

  ## pull the Covid Status survey
  covid = get_data(token, form = "covid_status", "raw")
  ## keep subjects who reported having COVID during pregnancy
  covid = covid %>% filter(covid_preg_trimester>=1)
  covid = covid[, c("record_id", "covid_preg_trimester")]

  ## pull the NCIPR survey
  ncipr = get_data(token, "novel_coronavirus_covid_illness_patient_report_nci", "raw")
  ## keep subjects who reported having COVID during pregnancy
  ncipr = ncipr %>% filter(ncipr_pregnant_any==1)
  ncipr = ncipr[, c("record_id", "ncipr_pregnant_any")]

  # merge all data sets with covid positive record IDs
  covid_pos = merge(baseline, ncipr, by="record_id", all.x=T, all.y=T)
  covid_pos = merge(covid_pos, covid, by="record_id", all.x=T, all.y=T)

  # delete duplicate record IDs
  covid_pos = covid_pos[!duplicated(covid_pos), ]

  # get record IDs from moms who provided infant data
  infant_data = get_data(token, "infant_demographics")
  infant_data = infant_data[,c("record_id")]

  # cross reference covid positive moms with moms who also provided any infant data
  covid_pos_infant_consent = merge(covid_pos, infant_data, by="record_id")
  covid_pos_infant_consent = covid_pos_infant_consent[!duplicated(covid_pos_infant_consent$record_id), ]
  return(covid_pos_infant_consent)
}


#' Process CHAOS data
#'
#' This function will download and compute total scores for the
#' CHAOS scale
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the completed surveys
#' @export
get_chaos <- function(token, timepoint = "infant_9months_arm_1") {
  chaos = get_data(token, "confusion_hubbub_and_order_scale_chaos")
  chaos = dplyr::filter(chaos, redcap_event_name == timepoint)
  chaos$chaos_late = abs(5 - chaos$chaos_late)
  chaos$chaos_zoo = abs(5 - chaos$chaos_zoo)
  chaos$chaos_fuss = abs(5 - chaos$chaos_fuss)
  chaos$chaos_plans = abs(5 - chaos$chaos_plans)
  chaos$chaos_think = abs(5 - chaos$chaos_think)
  chaos$chaos_arguement = abs(5 - chaos$chaos_arguement)
  chaos$chaos_rushed = abs(5 - chaos$chaos_rushed)

  chaos$chaos_score =  rowMeans(chaos[,c("chaos_late", "chaos_commotion","chaos_rushed", "chaos_interrupt", "chaos_plans",
                                         "chaos_arguement", "chaos_routine","chaos_findthings", "chaos_ontop",
                                         "chaos_zoo", "chaos_fuss", "chaos_think", "chaos_rushed", "chaos_relax", "chaos_calm")], na.rm=T)
  chaos = dplyr::rename(chaos, chaos_timestamp = confusion_hubbub_and_order_scale_chaos_timestamp)
  chaos = chaos[,c("record_id", "redcap_event_name", "chaos_timestamp", "chaos_score")]
  return (chaos)
}



#' Process ASQ data
#'
#' This function will download and compute scores for
#' all ASQ subscales at 6 months. Only total scores, and
#' not cut off values, are returned.
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the processed ASQ data
#' @export
get_asq <- function(token) {
  asq = get_data(token, "asq3_6_month_questionnaire")
  asq$comm = rowSums(asq[,7:12],na.rm=TRUE)
  asq$grossmotor = rowSums(asq[,13:18],na.rm=TRUE)
  asq$finemotor = rowSums(asq[,19:24],na.rm=TRUE)
  asq$probsolve = rowSums(asq[,25:30],na.rm=TRUE)
  asq$social = rowSums(asq[,31:36],na.rm=TRUE)
  asq = asq[,c("record_id", "asq3_6_month_questionnaire_timestamp","comm", "grossmotor", "finemotor", "probsolve", "social")]
  return (asq)
}

#' Process Perceived Stress data
#'
#' This function will download and return the perceived
#' stress scores.
#'
#' @param token Unique REDCap token ID
#' @param timepoint Survey timepoint branch requested
#' @return A data frame for the completed surveys
#' @export
get_pss <- function(token, timepoint = "infant_6months_arm_1") {
  pss = get_data(token, "perceived_stress_scale_pss")
  pss = dplyr::filter(pss, redcap_event_name == timepoint)
  pss = pss[,c("record_id", "perceived_stress_scale_pss_timestamp", "pss_score", "pss_14_score")]
  return (pss)
}

#' Process EPDS postpartum depression data
#'
#' This function will download and return the total
#' summed scores for the EPDS. Only total scores, and
#' not cutoff values, are returned.
#'
#' @param token Unique REDCap token ID
#' @param timepoint Survey timepoint branch requested
#' @return A data frame for the completed surveys
#' @export
get_epds <- function(token, timepoint = "infant_6months_arm_1") {
  epds = get_data(token, "cope_epds")
  epds = dplyr::filter(epds, redcap_event_name == timepoint)

  epds$epds_1_v2 = 3 - epds$epds_1_v2
  epds$epds_2_v2 = 3 - epds$epds_2_v2
  epds$epds_all = epds$epds_1_v2 + epds$epds_2_v2 + epds$epds_3_v2 + epds$epds_4_v2 + epds$epds_5_v2 +
    epds$epds_6_v2 + epds$epds_7_v2 + epds$epds_8_v2 + epds$epds_9_v2 + epds$epds_10_v2

  epds$epds_dep = epds$epds_1_v2 + epds$epds_2_v2 + epds$epds_3_v2 +
    epds$epds_7_v2 + epds$epds_8_v2 + epds$epds_9_v2 + epds$epds_10_v2

  epds$epds_anx = epds$epds_4_v2 + epds$epds_5_v2 + epds$epds_6_v2

  epds = epds[,c("record_id", "cope_epds_timestamp","epds_all", "epds_dep", "epds_anx")]
  return (epds)
}

#' Process Trauma History data
#'
#' This function will download and return the total number
#' of traumatic events reported by subjects.
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the completed surveys
#' @export
get_thq <- function(token) {
  thq = get_data(token, "thq")
  thq[["thq_events_number"]][is.na(thq[["thq_events_number"]])] <- 0
  thq[["thq_24"]][is.na(thq[["thq_24"]])] <- 0
  thq[thq[, "thq_24"] == 1,"thq_events_number"] = thq[thq[, "thq_24"] == 1,"thq_events_number"] + 1
  thq <- thq[,c("record_id", "thq_timestamp","thq_events_number")]
  return (thq)
}

#' Process Parenting Reward Data
#'
#' This function will download and return the average
#' parenting reward scores.
#'
#' @param token Unique REDCap token ID
#' @param timepoint Survey timepoint branch requested
#' @return A data frame for the completed surveys
#' @export
get_parent_reward <- function(token, timepoint = "infant_3months_arm_1") {
  parent_reward = get_data(token, "parenting_reward_questionnaire", form_complete = T)
  parent_reward =  dplyr::filter(parent_reward, redcap_event_name == timepoint)
  parent_reward$scale_reward = parent_reward$scale_reward/2
  parent_reward$prq_matrix_q06r = abs(parent_reward$prq_matrix_q06 - 6)
  parent_reward$parenting_reward_avg = rowMeans(parent_reward[, c("prq_matrix_q01","prq_matrix_q02","prq_matrix_q03","prq_matrix_q04",
                                                                  "prq_matrix_q05","prq_matrix_q06r", "prq_matrix_q07","prq_matrix_q08","prq_matrix_q09",
                                                                  "prq_matrix_q10","prq_matrix_q11","prq_matrix_q12","prq_matrix_q13",
                                                                  "prq_matrix_q14", "scale_reward")], na.rm=T)
  parent_reward = parent_reward[,c("record_id", "parenting_reward_questionnaire_timestamp", "parenting_reward_avg")]
  return(parent_reward)
}

#' Process Reward Responsivity data
#'
#' This function will download and return mother's
#' average reward responsivity scores
#'
#' @param token Unique REDCap token ID
#' @param timepoint Survey timepoint branch requested
#' @return A data frame for the completed surveys
#' @export
get_reward <- function(token, timepoint = "infant_3months_arm_1") {
  reward = get_data(token, "reward_responsivity_questionnaire")
  reward = dplyr::filter(reward, redcap_event_name == timepoint)
  rew_start = which( colnames(reward)=="rrq_matrix_q01" )
  rew_end = rew_start+7
  reward$reward_avg = rowMeans(reward[,rew_start:rew_end],na.rm=T)
  reward = reward[,c("record_id","reward_responsivity_questionnaire_timestamp", "reward_avg")]
  return(reward)
}

#' Process 12 month BITSEA data
#'
#' This function will download and return total scores
#' and cut off values for the competence, problems,
#' and autism subscales of the BITSEA survey at 12 months.
#'
#' @param token Unique REDCap token ID
#' @param timepoint Survey timepoint branch requested
#' @return A data frame for the completed surveys
#' @export
get_bitsea <- function(token, timepoint = "infant_12months2_arm_1") {
  bitsea = get_data(token, "brief_infanttoddler_social_and_emotional_assessmen")
  bitsea = dplyr::filter(bitsea, redcap_event_name == timepoint)
  bitsea$autism_competence  = rowSums(bitsea[,c("bitsea_1", "bitsea_10", "bitsea_13", "bitsea_15",
                                                "bitsea_22", "bitsea_25", "bitsea_29", "bitsea_031")], na.rm=T)
  bitsea$autism_problems = rowSums(bitsea[,c("bitsea_9", "bitsea_14", "bitsea_21", "bitsea_35",
                                             "bitsea_36", "bitsea_37", "bitsea_38", "bitsea_39",
                                             "bitsea_40")], na.rm=T)
  bitsea$autism_total = bitsea$autism_problems - bitsea$autism_competence
  bitsea$problems = rowSums(bitsea[,c("bitsea_2", "bitsea_3", "bitsea_4", "bitsea_6", "bitsea_7", "bitsea_8",
                                      "bitsea_9", "bitsea_11", "bitsea_12", "bitsea_14", "bitsea_16", "bitsea_17",
                                      "bitsea_18", "bitsea_21", "bitsea_23", "bitsea_24", "bitsea_26", "bitsea_27",
                                      "bitsea_28", "bitsea_30", "bitsea_032", "bitsea_033", "bitsea_034", "bitsea_35",
                                      "bitsea_36", "bitsea_37", "bitsea_38", "bitsea_39", "bitsea_40",
                                      "bitsea_041", "bitsea_042")], na.rm=T)
  bitsea$competence = rowSums(bitsea[,c("bitsea_1", "bitsea_5", "bitsea_10", "bitsea_13", "bitsea_15",
                                        "bitsea_19", "bitsea_20", "bitsea_22", "bitsea_25", "bitsea_29", "bitsea_031")], na.rm=T)
  bitsea = bitsea[,c("record_id", "brief_infanttoddler_social_and_emotional_assessmen_timestamp","autism_competence", "autism_problems", "autism_total", "competence", "problems")]
  bitsea$problem_thres = 0
  bitsea[bitsea$problems > 11,"problem_thres"] = 1
  bitsea$competence_thres = 0
  bitsea[bitsea$competence < 13,"competence_thres"] = 1
  bitsea$autism_total_thresh = 0
  bitsea[bitsea$autism_total > 7,"autism_total_thresh"] = 1
  bitsea$autism_comp_thresh = 0
  bitsea[bitsea$competence < 12,"autism_comp_thresh"] = 1
  bitsea$autism_prob_thresh = 0
  bitsea[bitsea$autism_problems > 4,"autism_prob_thresh"] = 1
  return (bitsea)
}

#' Process Parenting Stress Index data
#'
#' This function will download and return the average
#' parenting stress scores from the PSI.
#'
#' @param token Unique REDCap token ID
#' @param timepoint Survey timepoint branch requested
#' @return A data frame for the completed surveys
#' @export
get_parent_stress <- function(token, timepoint = "infant_9months_arm_1") {
  parent_stress = get_data(token, form = "parenting_stress_index_fourth_edition_short_form_p")
  parent_stress = parent_stress %>% dplyr::filter(redcap_event_name == timepoint)
  parent_stress$psi = rowMeans(parent_stress[, 7:42], na.rm=T)
  parent_stress = parent_stress[,c("record_id", "parenting_stress_index_fourth_edition_short_form_p_timestamp","psi")]
  return (parent_stress)
}


#' Process Maternal Postnatal Attachment Scale
#'
#' This function will download and return average
#' scores for the pleasure and quality subscales
#' of the MPAS.
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the completed surveys
#' @export
get_mpas <- function(token) {
  mpas = get_data(token, "maternal_postnatal_attachment_scale_mpas")
  mpas$mpas_7 = 5 - mpas$mpas_7
  mpas$mpas_10 = 5 - mpas$mpas_10
  mpas$mpas_14 = 4 - mpas$mpas_14
  mpas$pleasure = rowMeans(mpas[,c("mpas_9","mpas_10", "mpas_11", "mpas_13")], na.rm=T)
  mpas$quality = rowMeans(mpas[,c("mpas_3", "mpas_4","mpas_5", "mpas_6", "mpas_7",
                                  "mpas_10", "mpas_14","mpas_18", "mpas_19")], na.rm=T)
  mpas = mpas[,c("record_id", "maternal_postnatal_attachment_scale_mpas_timestamp","pleasure", "quality")]
  return (mpas)

}

#' Process Parenting Role Strain Data
#'
#' This function will download and return average
#' parenting role strain scores, only for the questions
#' probing CURRENT strain and NOT past strain
#'
#' @param token Unique REDCap token ID
#' @param timepoint Survey timepoint branch requested
#' @return A data frame for the completed surveys
#' @export
get_parent_strain <- function(token, timepoint = "infant_6months_arm_1") {
  parent_strain = get_data(token, "parenting_stress_role_strain_covid", form_complete = T)
  parent_strain =  dplyr::filter(parent_strain, redcap_event_name == timepoint)
  cols = c(1,6,8,10,12,14,16,18,20,22)
  parent_strain = parent_strain[,cols]
  parent_strain$strain = rowMeans(parent_strain[,3:10], na.rm=T)
  parent_strain = parent_strain[,c("record_id", "parenting_stress_role_strain_covid_timestamp","strain")]
}

#' Process IBQ data
#'
#' This function will download and process
#' the effortful control, negative affect,
#' and surgency subscales of the infant
#' behavior record.
#'
#' @param token Unique REDCap token ID
#' @param timepoint Survey timepoint branch requested
#' @return A data frame for the completed surveys
#' @export
get_ibq <- function(token, timepoint = "infant_6months_arm_1") {
  ibq = get_data(token, "infant_behavior_questionnaire_very_short_form")
  ibq = dplyr::filter(ibq, redcap_event_name == timepoint)
  ibq$ibqr_11_r = 8 - ibq$ibqr_11
  ibq$ibq_ec = rowMeans(ibq[, c("ibqr_05","ibqr_06","ibqr_11_r","ibqr_12","ibqr_18","ibqr_19",
                                "ibqr_24","ibqr_25","ibqr_30","ibqr_31","ibqr_34","ibqr_35")], na.rm=T)
  ibq$ibq_neg = rowMeans(ibq[, c("ibqr_03","ibqr_04","ibqr_09","ibqr_10","ibqr_16","ibqr_17",
                                 "ibqr_22","ibqr_23","ibqr_28","ibqr_29","ibqr_32","ibqr_33")], na.rm=T)
  ibq$ibq_sur = rowMeans(ibq[, c("ibqr_01","ibqr_02","ibqr_07","ibqr_08","ibqr_13","ibqr_14",
                                 "ibqr_15","ibqr_20","ibqr_21","ibqr_26","ibqr_27","ibqr_36", "ibqr_37")], na.rm=T)
  ibq = ibq[,c("record_id", "infant_behavior_questionnaire_very_short_form_timestamp","ibq_sur", "ibq_neg", "ibq_ec")]
}

#' Process Baseline data
#'
#' This function will download and return the
#' full data for all subjects who completed
#' the baseline COPE survey.
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the completed surveys
#' @export
get_baseline <- function (token) {
  baseline = get_data(token, "covid_19_survey_all", raw_v_label = 'raw')
  return (baseline)
}


#' Process Infant Care data
#'
#' This function will download and return the
#' Infant Care Questionnaire dataset
#'
#' @param token Unique REDCap token ID
#' @param timepoint Survey timepoint branch requested
#' @return A data frame for the completed surveys
#' @export
get_infant_care <- function(token, timepoint = "infant_6months_arm_1") {
  infant_care = get_data(token, "infant_care_questionnaire")
  infant_care =  dplyr::filter(infant_care, redcap_event_name == timepoint)
#  infant_care$parenting_attention = rowMeans(infant_care[, c("icq_3", "icq_6","icq_8","icq_9","icq_11","icq_13")], na.rm=T)
#  infant_care$infant_reward_response = rowMeans(infant_care[, c("icq_16","icq_19","icq_20")], na.rm=T)
#  infant_care = infant_care[,c("record_id", "parenting_attention", "infant_reward_response")]
  return (infant_care)
}

#' Process Baseline BSI data
#'
#' This function will calculate the depression, anxiety,
#' somatic, and global subscales of the BSI, and the
#' PTSD score of the PCL-5 from the baseline data set.
#'
#' @param baseline Data frame containing the baseline survey data
#' @return A data frame containing the BSI and PTSD scores
#' @export
baseline_BSI <- function (baseline) {
  baseline$bsi_dep_avg = rowMeans(baseline[,c("bsi_no_interest", "bsi_feel_lonely", "bsi_feeling_blue",
                                              "bsi_worthless", "bsi_hopeless")], na.rm=T)

  baseline$bsi_anx_avg = rowMeans(baseline[, c("bsi_nervous_shaky", "bsi_feel_tense", "bsi_scared", "bsi_terror_spells",
                                               "bsi_restless", "bsi_fearful")], na.rm=T)

  baseline$bsi_som_avg = rowMeans(baseline[, c("bsi_heart_pain", "bsi_nausea", "bsi_breathless", "bsi_numbness",
                                               "bsi_faint_dizzy", "bsi_weak_body")], na.rm=T)

  baseline$bsi_global_avg = rowMeans(baseline[c("bsi_dep_avg", "bsi_anx_avg", "bsi_som_avg")], na.rm=TRUE)

  baseline$ptsd_avg = rowMeans(baseline[c("ptsd_alert", "ptsd_jumpy", "ptsd_no_concentr", "ptsd_thoughts", "ptsd_dreams",
                                          "ptsd_no_posit", "ptsd_guilty", "ptsd_irritable", "ptsd_risks", "ptsd_avoid")], na.rm=TRUE)

  bsi = baseline[, c("record_id", "bsi_dep_avg", "bsi_anx_avg", "bsi_som_avg", "bsi_global_avg", "ptsd_avg")]

  return (bsi)
}

#' Process Baseline SES data
#'
#' This function will recode and return income and education
#' brackets from the baseline survey.
#'
#' Income is coded as 1 = < 10k, 2 = 10-30k, 3 = 30-50k, 4 = 50-80k,
#' 5 = 80-120k, 6 = 120-160k, 7 = 160-200k, 8 = 200-250k, 9 = 250k+.
#'
#' Education is coded as 1 = less than high school/GED, 2 = high school/GED, 3 = trade
#' school or partial college, 4 = 2-year degree, 5 = 4-year degree, 6 = graduate degree
#'
#' @param baseline Data frame containing the baseline survey data
#' @return A data frame containing binned income and education values
#' @export
baseline_SES <- function (baseline) {

  baseline$income_binned = NA
  baseline$income_binned[baseline$income == 1] <- 1
  baseline$income_binned[baseline$income == 2  | baseline$income == 3] <- 2
  baseline$income_binned[baseline$income == 16 | baseline$income == 4] <- 3
  baseline$income_binned[baseline$income == 5 | baseline$income == 6] <- 4
  baseline$income_binned[baseline$income == 7 | baseline$income == 8] <- 5
  baseline$income_binned[baseline$income == 9 | baseline$income == 10] <- 6
  baseline$income_binned[baseline$income == 11 | baseline$income == 12] <- 7
  baseline$income_binned[baseline$income == 13 | baseline$income == 14] <- 8
  baseline$income_binned[baseline$income == 15] <- 9

  baseline$education_binned = NA
  baseline$education_binned[baseline$education < 3] <- 1
  baseline$education_binned[baseline$education == 3] <- 2
  baseline$education_binned[baseline$education == 4 | baseline$education == 5] <- 3
  baseline$education_binned[baseline$education == 6] <- 4
  baseline$education_binned[baseline$education == 7] <- 5
  baseline$education_binned[baseline$education == 8] <- 6
  ses = baseline[,c("record_id", "income_binned", "education_binned")]
  return (ses)
}

#' Pull a Specifield Survey Field
#'
#' This function will return redcap_event_name and field data for each event
#'
#' @param token Unique REDCap token ID
#' @param field Name of the specific field to be downloaded
#' @param raw_v_label Whether raw data or labels are requested
#' @return A data frame for the completed record_ids, redcap_event_name and field
#' @export
get_field <- function(token = token, field=field, raw_v_label = 'raw') {
  formData <- list(uri = url,
                   "token"=token,
                   content='record',
                   format='csv',
                   type='flat',
                   csvDelimiter='',
                   'fields[0]'='record_id',
                   'fields[1]' = field,
                   rawOrLabel=raw_v_label,
                   rawOrLabelHeaders=raw_v_label,
                   exportCheckboxLabel='false',
                   exportSurveyFields='false',
                   exportDataAccessGroups='false',
                   returnFormat='csv')

  response <- httr::POST(url, body = formData, encode = "form")
  df <- httr::content(response)
  df[df == -888] = NA
  df[df == 888] = NA
  df[df == 8888] = NA
  df[df == -999] = NA
  df[df == 999] = NA
  df[df == 9999] = NA
  df <- dplyr::select(df, record_id, redcap_event_name, field)
  df <- df[!is.na(df[[field]]),]
  return (df)
}


#' Generating N for Specified Timepoint
#'
#' This function with return a numeric variable for the number of people who have completed a particular event
#'
#' @param token Unique REDCap token ID
#' @param timepoint redcap_event_name for the event you wish to pull
#' @return Numeric variable for that event
#' @export
get_visit_n <- function(token, timepoint = "infant_6months_arm_1") {
  visits = get_data(token, form = "infant_visit_date_and_age_tracker", form_complete = T)
  visits = dplyr::filter(visits, redcap_event_name == timepoint)
  n = nrow(visits)
  return (n)
}


#' Generates data for specified timepoint
#'
#' This function returns a dataframe with infant dob, date, and age for a specified event
#'
#' @param token Unique REDCap token ID
#' @param timepoint redcap_event_name for the timepoint you wish to pull
#' @return dataframe with visit data for that timepoint
#' @export
get_visit_data <- function(token, timepoint = "infant_6months_arm_1") {
  visits = get_data(token, form = "infant_visit_date_and_age_tracker", form_complete = T)
  visits = dplyr::filter(visits, redcap_event_name == timepoint)
  visits = dplyr::select(visits, record_id, infant_visit_dob, infant_visit_date, infant_visit_age)
  return (visits)
}


#' Provides timepoint completion info for every timepoint
#'
#' @param token Unique REDCap token ID
#' @return dataframe with each ID, and a column for each timepoint with date of completion
#' @export
get_all_timepoints<- function(token) {
  visits <- get_data(token, form="infant_visit_date_and_age_tracker", form_complete = T)
  visits <- dplyr::select(visits, record_id, redcap_event_name, infant_visit_date)
  visits$infant_visit_date <- as.Date(visits$infant_visit_date, format="%Y-%m-%d")
  visits <- tidyr::pivot_wider(visits, names_from = 'redcap_event_name',
                               values_from = 'infant_visit_date', id_cols = 'record_id')
  names(visits)[-1] <- gsub("_arm_1", "", names(visits)[-1])
  visits <- dplyr::rename(visits, infant_12months = infant_12months2)
  visits <- dplyr::select(visits, record_id, infant_6months, infant_9months, infant_12months, infant_18months, infant_30months, child_42months)
  return(visits)
}


#' Generates REDCap Survey Data
#'
#' Returns a data frame with contact info, completion status, access codes etc. for every ID for a given survey
#'
#' @param token Unique REDCap token ID
#' @param form Name of the survey to be downloaded
#' @param event Event name e.g. infant_30months_arm_1
#' @return Data frame with record_ids and survey information for particular form
#' @export
#'
get_survey_data <- function(token, form=form, event=event) {
  formData <- list("token" = token,
                   content='participantList',
                   format = 'csv',
                   instrument = form,
                   event = event,
                   returnFormat='csv')
  response <- httr::POST(url, body = formData, encode="form")
  df <- httr::content(response)
  df <- dplyr::filter(df, !is.na(record))
  df <- dplyr::rename(df, record_id = record)
  df <- dplyr::select(df, record_id, email, phone, invitation_sent_status:survey_queue_link)
  return(df)
}


#' Generates Child DOB Info
#'
#' Parses through all child DOB variable, removes errors, returns final date & age in days
#' For IDs with no DOB data, also returns due date & expected age based on that
#' For IDs with only 2 DOB entries, both different, the earliest will be chosen & a message printed indicating that ID should be checked
#'
#' @param token Unique REDCap token ID
#' @return data frame of record id and most recent/accurate child_dob variable & IDs to check manually
#' @export
get_child_dob <- function(token) {
  #child dob variable 1: mhx_babydob (3)
  mhx_babydob <- get_field(token, field="mhx_babydob")
  mhx_babydob$redcap_event_name <- gsub("infant_visits_arm_1", "1", mhx_babydob$redcap_event_name)
  mhx_babydob$redcap_event_name <- gsub("infant_30months_arm_1", "2", mhx_babydob$redcap_event_name)
  mhx_babydob$redcap_event_name <- gsub("child_42months_arm_1", "3", mhx_babydob$redcap_event_name)
  mhx_babydob <- tidyr::pivot_wider(mhx_babydob, id_cols = record_id, names_from = redcap_event_name, values_from = mhx_babydob, names_prefix = "mhx_babydob_")
  #child dob variable 2: welcome_30 (1)
  welcome_30 <- get_field(token, field="month30_dob_welcome")
  welcome_30 <- dplyr::select(welcome_30, record_id, month30_dob_welcome)
  #child dob variable 3: infant_visit_dob (6)
  infant_visit_dob <- get_field(token, field="infant_visit_dob")
  infant_visit_dob$redcap_event_name <- gsub("infant_6months_arm_1", "1", infant_visit_dob$redcap_event_name)
  infant_visit_dob$redcap_event_name <- gsub("infant_9months_arm_1", "2", infant_visit_dob$redcap_event_name)
  infant_visit_dob$redcap_event_name <- gsub("infant_12months2_arm_1", "3", infant_visit_dob$redcap_event_name)
  infant_visit_dob$redcap_event_name <- gsub("infant_18months_arm_1", "4", infant_visit_dob$redcap_event_name)
  infant_visit_dob$redcap_event_name <- gsub("infant_30months_arm_1", "5", infant_visit_dob$redcap_event_name)
  infant_visit_dob$redcap_event_name <- gsub("child_42months_arm_1", "6", infant_visit_dob$redcap_event_name)
  infant_visit_dob <- tidyr::pivot_wider(infant_visit_dob, id_cols = record_id, names_from = redcap_event_name, values_from = infant_visit_dob, names_prefix = "infant_visit_dob_")
  infant_visit_dob <- dplyr::select(infant_visit_dob,record_id, infant_visit_dob_1, infant_visit_dob_2, infant_visit_dob_3, infant_visit_dob_4, infant_visit_dob_5, infant_visit_dob_6)
  #child dob variable 4: bio_dob (1)
  bio_dob <- get_field(token, field="bio_dob")
  bio_dob <- dplyr::select(bio_dob, -redcap_event_name)
  #child dob variable 5: child_birth_date2
  child_birth_date <- get_field(token, field = "child_birth_date2")
  child_birth_date$redcap_event_name <- gsub("covid_week_2_follo_arm_1", "1", child_birth_date$redcap_event_name)
  child_birth_date$redcap_event_name <- gsub("covid_week_4_follo_arm_1", "2", child_birth_date$redcap_event_name)
  child_birth_date$redcap_event_name <- gsub("covid_week_6_follo_arm_1", "3", child_birth_date$redcap_event_name)
  child_birth_date$redcap_event_name <- gsub("covid_week_8_follo_arm_1", "4", child_birth_date$redcap_event_name)
  child_birth_date$redcap_event_name <- gsub("covid_week_10_foll_arm_1", "5", child_birth_date$redcap_event_name)
  child_birth_date <- tidyr::pivot_wider(child_birth_date, id_cols = record_id, names_from = redcap_event_name, values_from = child_birth_date2, names_prefix = "child_birth_date_")
  #child dob variable 6: child_birth_date
  child_birth_date2 <- get_field(token, field = "child_birth_date")
  child_birth_date2$redcap_event_name <- gsub("baseline_2021_arm_1", "1", child_birth_date2$redcap_event_name)
  child_birth_date2$redcap_event_name <- gsub("covid_arm_1", "2", child_birth_date2$redcap_event_name)
  child_birth_date2 <- tidyr::pivot_wider(child_birth_date2, id_cols = record_id, names_from = redcap_event_name, values_from = child_birth_date, names_prefix = "child_birth_date2_")
  #child dob variable 7: re_child_dob (1)
  re_child_dob <- get_field(token, field="re_child_dob")
  re_child_dob <- dplyr::select(re_child_dob, -redcap_event_name)
  
  #cleaning child birth date responses
  child_birth_date <- dplyr::full_join(child_birth_date, child_birth_date2, by="record_id")
  cols <- colnames(child_birth_date[2:7])
  child_birth_date[cols] <- lapply(child_birth_date[cols], function(col) {
    ifelse(col > Sys.Date() | col < "2019-01-01", NA_real_, col)
  })
  child_birth_date[cols] <- lapply(child_birth_date[cols], as.Date, origin="1970-01-01")
  for (i in 1:nrow(child_birth_date)) {
    row_vec <- na.omit(as.numeric(child_birth_date[i, 2:7]))
    row_vec <- as.Date(row_vec, origin="1970-01-01")
    
    if (length(row_vec) > 1) {row_vec <- row_vec[duplicated(row_vec)]}
    child_birth_date[i, "child_birth_date"] <- row_vec[1]
  }
  child_birth_date <- dplyr::select(child_birth_date, record_id, child_birth_date)
  
  #due date variables 
  pregnant_due_date <- get_field(token, field="pregnant_due_date")
  pregnant_due_date <- dplyr::distinct(pregnant_due_date, record_id, .keep_all=T)
  pregnant_due_date <- dplyr::select(pregnant_due_date, -redcap_event_name)
  bio_edd <- get_field(token, field="bio_edd")
  bio_edd <- dplyr::select(bio_edd, -redcap_event_name)
  
  #creating dob variable
  dobs <- dplyr::full_join(mhx_babydob, welcome_30, by="record_id")
  dobs <- dplyr::full_join(dobs, infant_visit_dob, by="record_id")
  dobs <- dplyr::full_join(dobs, bio_dob, by="record_id")
  dobs <- dplyr::full_join(dobs, child_birth_date, by="record_id")
  dobs <- dplyr::full_join(dobs, re_child_dob, by="record_id")
  dobs <- dplyr::full_join(dobs, pregnant_due_date, by="record_id")
  dobs <- dplyr::full_join(dobs, bio_edd, by="record_id")
  cols <- colnames(dobs[2:16])
  dobs[cols] <- lapply(dobs[cols], as.Date, format = "%Y-%m-%d")
  dobs[cols] <- lapply(dobs[cols], function(col) {
    ifelse(col > Sys.Date() | col < "2019-01-01", NA_real_, col)
  })
  dobs[cols] <- lapply(dobs[cols], as.Date, origin="1970-01-01")
  
  for (i in 1:nrow(dobs)) {
    row_vec <- na.omit(as.numeric(dobs[i, 2:14]))
    row_vec <- as.Date(row_vec, origin="1970-01-01")
    if (length(row_vec) > 1) {row_vec <- row_vec[duplicated(row_vec)]}
    dobs[i, "final_child_dob"] <- row_vec[1]
  }
  
  for (i in 1:nrow(dobs)) {
    if (is.na(dobs$final_child_dob[i])) {
      row_vec <- na.omit(as.numeric(dobs[i, 2:14]))
      col_names <- names(dobs[, 2:14])[!is.na(dobs[i, 2:14])]
      row_vec <- as.Date(row_vec, origin="1970-01-01")
      if (length(row_vec) > 1) {
        dobs[i, "final_child_dob"] <- min(row_vec)
        message(paste("Discrepancy for ID ", dobs$record_id[i]), " - earliest date used, double check")
      }
    }
  }
  
  dobs$due_date <- ifelse(!is.na(dobs$pregnant_due_date), dobs$pregnant_due_date, dobs$bio_edd)
  dobs$due_date <- as.Date(dobs$due_date, origin="1970-01-01")
  
  dobs <- dplyr::select(dobs, record_id, final_child_dob, due_date)
  
  dobs$child_current_age <- as.numeric(difftime(Sys.Date(), dobs$final_child_dob, units = "days"))
  dobs$expected_child_age <- as.numeric(difftime(Sys.Date(), dobs$due_date, units="days"))
  dobs <- dplyr::rename(dobs, child_dob_final = final_child_dob)
  return(dobs)
}

#'Generates Mom DOB Info
#'
#' Parses through all mom DOB variable, removes errors, returns final date & current age
#' For IDs with only 2 DOB entries, both different, the earliest will be chosen & a message printed indicating that ID should be checked
#'
#' @param token Unique REDCap token ID
#' @return data frame of record id and most recent/accurate mom_dob variable & IDs to check manually
#' @export
get_mom_dob <- function(token) {
  birth_date <- get_field(token, field = "birth_date")
  birth_date <- dplyr::select(birth_date, -redcap_event_name)
  mom_demo_momdob <- get_field(token, field = "mom_demo_momdob")
  mom_demo_momdob$redcap_event_name <- gsub("infant_visits_arm_1", "1", mom_demo_momdob$redcap_event_name)
  mom_demo_momdob$redcap_event_name <- gsub("infant_30months_arm_1", "2", mom_demo_momdob$redcap_event_name)
  mom_demo_momdob <- tidyr::pivot_wider(mom_demo_momdob, id_cols = record_id, names_from = redcap_event_name, values_from = mom_demo_momdob, names_prefix = "mom_demo_momdob_")
  re_mom_dob <- get_field(token, field="re_mom_dob")
  re_mom_dob <- dplyr::select(re_mom_dob, -redcap_event_name)
  
  #merging into one data frame
  mom_dob <- dplyr::full_join(birth_date, mom_demo_momdob, by="record_id")
  mom_dob <- dplyr::full_join(mom_dob, re_mom_dob, by="record_id")
  cols <- c("birth_date", "mom_demo_momdob_1", "mom_demo_momdob_2", "re_mom_dob")
  mom_dob[cols] <- lapply(mom_dob[cols], as.Date, format = "%Y-%m-%d")
  
  #removing values that are incorrect
  mom_dob$birth_date <- ifelse(mom_dob$birth_date > "2010-01-01" | mom_dob$birth_date < "1950-01-01", NA_real_, mom_dob$birth_date)
  mom_dob$mom_demo_momdob_1 <- ifelse(mom_dob$mom_demo_momdob_1 > "2010-01-01" | mom_dob$mom_demo_momdob_1 < "1950-01-01", NA_real_, mom_dob$mom_demo_momdob_1)
  mom_dob$mom_demo_momdob_2 <- ifelse(mom_dob$mom_demo_momdob_2 > "2010-01-01" | mom_dob$mom_demo_momdob_2 < "1950-01-01", NA_real_, mom_dob$mom_demo_momdob_2)
  mom_dob$re_mom_dob <- ifelse(mom_dob$re_mom_dob > "2010-01-01" | mom_dob$re_mom_dob < "1950-01-01", NA_real_, mom_dob$re_mom_dob)
  mom_dob[cols] <- lapply(mom_dob[cols], as.Date, origin="1970-01-01")
  
  #making final dob column for rows where there are no discrepancies
  for (i in 1:nrow(mom_dob)) {
    row_vec <- na.omit(as.numeric(mom_dob[i, 2:5]))
    row_vec <- as.Date(row_vec, origin="1970-01-01")
    if (length(row_vec) > 1) {row_vec <- row_vec[duplicated(row_vec)]}
    mom_dob[i, "mom_dob_final"] <- row_vec[1]
  }
  
  #selecting dob when there is a discrepeancy
  for (i in 1:nrow(mom_dob)) {
    if (is.na(mom_dob$mom_dob_final[i])) {
      row_vec <- na.omit(as.numeric(mom_dob[i, 2:5]))
      col_names <- names(mom_dob[, 2:5])[!is.na(mom_dob[i, 2:5])]
      row_vec <- as.Date(row_vec, origin="1970-01-01")
      if (length(row_vec) > 1) {
        mom_dob[i, "mom_dob_final"] <- min(row_vec)
        message(paste("Discrepancy for ID ", mom_dob$record_id[i]), " - earliest date used, double check")
      }
    }
  }
  
  #creating final dataset
  mom_dob <- dplyr::select(mom_dob, record_id, mom_dob_final)
  mom_dob$mom_current_age <- as.numeric(difftime(Sys.Date(), mom_dob$mom_dob_final, units="days")/365.25)
  return(mom_dob)
}


#' Generates Expected Invite Data
#'
#' Lists all expected invite dates/months for specified timepoints and time frames
#' 
#' @param token Unique REDCap token ID
#' @param timepoint The COPE timepoint in numeric format (6, 9, 12, 18, 30, 42)
#' @param max_date The max date you wish to pull data for (e.g. '2024-06-01'), default is all future invites
#' @return A list with 1) data frame of all ids, invite date, month, longitudinal status, 2) stacked bar chart of counts by month, 3) data frame of counts by month
#' @export
get_expected_invites <- function(token, timepoint = timepoint, max_date = 'none') {
  library(dplyr)

  #creating data frame 
  age_in = case_when(
    timepoint == 6 ~ 142, timepoint == 9 ~ 243, timepoint == 12 ~ 336, timepoint == 18 ~ 504, timepoint == 30 ~ 870, timepoint == 42 ~ 1232
  )
  
  all_dobs <- get_child_dob(token)
  
  longitudinal_participants <- get_all_timepoints(token)
  longitudinal_participants <- longitudinal_participants %>%
    filter(!is.na(infant_6months) | !is.na(infant_12months) | !is.na(infant_30months) | is.na(child_42months)) %>%
    select(record_id) %>%
    mutate(longitudinal = 'Longitudinal') 
  
  all_dobs <- all_dobs %>%
    mutate(dob = ifelse(!is.na(child_dob_final) & !is.na(due_date), child_dob_final,
                        ifelse(is.na(child_dob_final) & !is.na(due_date), due_date, child_dob_final))) %>%
    select(record_id, dob) %>%
    mutate(expected_invite_date = as.Date((dob + age_in), origin='1970-01-01'),
           dob = as.Date(dob, origin='1970-01-01'),
           invite_month = format(expected_invite_date, "%b %y")) %>%
    arrange(expected_invite_date) %>%
    filter(expected_invite_date >= Sys.Date()) %>%
    left_join(longitudinal_participants, by='record_id') 
  
  all_dobs$longitudinal <- ifelse(is.na(all_dobs$longitudinal), 'Not Longitudinal', all_dobs$longitudinal)
  
  if (max_date != 'none') {
    max_date = as.Date(max_date, format='%Y-%m-%d')
    
    all_dobs <- all_dobs %>%
      filter(expected_invite_date <= max_date)
  }
  
  #bar chart 
  all_dobs$invite_month <- factor(all_dobs$invite_month, levels = unique(all_dobs$invite_month))
  
  if ("Longitudinal" %in% all_dobs$longitudinal && "Not Longitudinal" %in% all_dobs$longitudinal) {
    colors <- c('#dabfff', '#907ad6')
  } else if ("Longitudinal" %in% all_dobs$longitudinal) {
    colors <- '#dabfff'
  } else if ("Not Longitudinal" %in% all_dobs$longitudinal) {
    colors <- '#907ad6'
  } else {
    # Handle other cases if needed
  }
  
  
  title = paste0("Future invites by month for ", timepoint, " month timepoint")
  
  plot <- ggplot(all_dobs, aes(x = invite_month, fill = factor(longitudinal))) +
    geom_bar(position = "stack", width = 0.8) +
    labs(title = title,
         x = "Month-Year",
         y = "Number of Invites",
         fill = "Longitudinal") + 
    theme(panel.background = element_rect(fill="white"),
          panel.border=element_rect(color="#3C1939", fill=NA, linewidth=1),
          legend.title = element_blank(), 
          axis.text = element_text(family = 'Arial', size = 10),
          axis.title = element_text(family='Arial', size=12),
          legend.text = element_text(family='Arial', size=11),
          title = element_text(family="Arial", size=14)) +
    scale_fill_manual(values = colors)
  plot
  
  #creating data frame with counts per month 
  counts <- data.frame(table(all_dobs$invite_month)) %>%
    rename(total = Freq)
  
  long_counts <- data.frame(table(subset(all_dobs, longitudinal == 'Longitudinal')$invite_month)) %>%
    rename(longitudinal = Freq)
  non_long_counts <-  data.frame(table(subset(all_dobs, longitudinal == 'Not Longitudinal')$invite_month)) %>%
    rename(non_longitudinal = Freq)
  
  counts <- counts %>%
    left_join(long_counts, by='Var1') %>%
    left_join(non_long_counts, by='Var1') %>%
    rename(month = Var1)
  
  #compiling and saving list
  result <- list(data = all_dobs, plot = plot, counts = counts)
  
  return(result)
}


#' Process Early Childhood Behavior Questionnaire Data
#'
#' This function will download and return the
#' Early Childhood Behavior Questionnaire dataset for 30 months
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the completed surveys
#' @export
get_ecbq <- function(token) {
  ecbq <- get_data(token, form='early_childhood_behavior_questionnaire_ecbq')
  #reverse scoring items
  ecbq$ecbq_escape_hugs <- 8 - ecbq$ecbq_escape_hugs #item 12
  ecbq$ecbq_tire_frequency <- 8 - ecbq$ecbq_tire_frequency #item 14
  ecbq$ecbq_easily_soothed <- 8 - ecbq$ecbq_easily_soothed #item 34
  
  #subscales
  neg <- c('ecbq_tags_clothes','ecbq_bothered_sound', 'ecbq_afraid_vehicles', 'ecbq_no_desire_to_enter',
           'ecbq_task_irritated', 'ecbq_temper_tantrum', 'ecbq_fiddle_frequency', 'ecbq_tearful_frequency', 'ecbq_seem_blue',
           'ecbq_public_space_cling', 'ecbq_cry_3_min', 'ecbq_easily_soothed')
  sur <- c('ecbq_quick_decision', 'ecbq_new_activity', 'ecbq_energy', 'ecbq_active_frequency', 'ecbq_run_house', 'ecbq_risk_enjoyment',
           'ecbq_rough_games', 'ecbq_loved_ones_visiting', 'ecbq_new_toy_excitement', 'ecbq_seek_out_company', 'ecbq_desire_to_interact',
           'ecbq_different_people')
  ec <- c('ecbq_forbidden_activity', 'ecbq_patience', 'ecbq_careful_breakable', 'ecbq_simultaneous_play', 'ecbq_attention_name',
          'ecbq_another_activity', 'ecbq_more_than_10_min', 'ecbq_tire_frequency', 'ecbq_escape_hugs', 'ecbq_mold_to_body',
          'ecbq_enjoy_singing', 'ecbq_smile_frequency')
  
  #scale means
  ecbq$ecbq_neg <- rowMeans(ecbq[, neg], na.rm = T)
  ecbq$ecbq_sur <- rowMeans(ecbq[, sur], na.rm = T)
  ecbq$ecbq_ec <- rowMeans(ecbq[, ec], na.rm = T)
  
  #making final dataset
  ecbq <- dplyr::select(ecbq, record_id, early_childhood_behavior_questionnaire_ecbq_timestamp,
                        ecbq_neg, ecbq_sur, ecbq_ec)
  
  return(ecbq)
}

#' Process 42m Childhood Behavior Questionnaire Data
#'
#' This function will download and return the
#' Childhood Behavior Questionnaire dataset for 42 months
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the completed surveys
#' @export
get_cbq <- function(token) {
  cbq <- get_data(token, form='child_behavior_questionnaire_very_short_form_cbqvs')
  
  #reverse scoring
  reversed <- c('cbq13', 'cbq19', 'cbq22', 'cbq31', 'cbq34', 'cbq20', 'cbq26', 'cbq29')
  cbq[reversed] <- lapply(cbq[reversed], function(x) 8 - x)
  
  neg <- c('cbq2', 'cbq5', 'cbq8', 'cbq11', 'cbq14', 'cbq17', 'cbq20', 'cbq23', 'cbq26', 'cbq29', 'cbq32', 'cbq35')
  sur <- c('cbq1', 'cbq4', 'cbq7', 'cbq10', 'cbq13', 'cbq16', 'cbq19', 'cbq22', 'cbq25', 'cbq28', 'cbq31', 'cbq34')
  ec <- c('cbq3', 'cbq6', 'cbq9', 'cbq12', 'cbq15', 'cbq18', 'cbq21', 'cbq24', 'cbq27', 'cbq30', 'cbq33', 'cbq36')
  
  #scale means
  cbq$cbq_neg <- rowMeans(cbq[, neg], na.rm = T)
  cbq$cbq_sur <- rowMeans(cbq[, sur], na.rm = T)
  cbq$cbq_ec <- rowMeans(cbq[, ec], na.rm = T)
  
  #making final dataset
  cbq <- dplyr::select(cbq, record_id, child_behavior_questionnaire_very_short_form_cbqvs_timestamp,
                       cbq_neg, cbq_sur, cbq_ec)
  
  return(cbq)
  
}

#' Process Social Support data
#'
#' This function will download and compute total scores for the
#' Social Support scale
#'
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name (default is 'infant_6months_arm_1')
#' @return A data frame for the completed surveys
#' @export
get_social_support <- function(token, timepoint = 'infant_6months_arm_1') {
  library(dplyr)
  ss = get_data(token, 'social_support')
  
  ss = filter(ss, redcap_event_name == timepoint)
  
  ss$ss_score_sum = rowSums(ss[, c('support3', 'support4', 'support5', 'support6', 'support7', 'support8', 'support9', 
                                   'support10', 'support11', 'support12', 'support13', 'support14', 'support15', 'support16',
                                   'support17', 'support18', 'support19', 'support20')], na.rm=T)
  
  ss$ss_score_mean = rowMeans(ss[, c('support3', 'support4', 'support5', 'support6', 'support7', 'support8', 'support9', 
                                     'support10', 'support11', 'support12', 'support13', 'support14', 'support15', 'support16',
                                     'support17', 'support18', 'support19', 'support20')], na.rm=T)
  
  ss$support1 <- as.numeric(ss$support1)
  ss$support2 <- as.numeric(ss$support2)
  
  ss <- ss %>%
    select(record_id, social_support_timestamp, support1, support2, ss_score_sum, ss_score_mean) %>%
    rename(ss_relatives_num = support1, ss_friends_num = support2,
           ss_timestamp = social_support_timestamp)
  
  
  return(ss)
  
}

#' Process PSQI data
#'
#' This function will download and compute total scores for the
#' Pit Sleep Quality Index scale
#'
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name (default is 'infant_6months_arm_1')
#' @return A data frame for the completed surveys
#' @export
get_pisq <- function(token, timepoint = 'infant_6months_arm_1') {
  library(dplyr)
  library(hms)
  sleep = get_data(token, "pit_sleep_quality_index")
  sleep = sleep %>% filter(redcap_event_name == timepoint)
  
  sleep$sleep_q4 = as.numeric(sleep$sleep_q4)
  max(sleep$sleep_q4,na.rm=T)
  
  ### HUGE OUTLIER (INCORRECT DATA ENTRY?), REMOVED AND FLAGGED AS MISSING DATA
  sleep[sleep$sleep_q4 == 1200, "sleep_q4"] = NA
  
  sleep[sleep$sleep_q2 <= 15, "sleep_q2_scored"] = 0
  sleep[sleep$sleep_q2 > 15 & sleep$sleep_q2 <= 30, "sleep_q2_scored"] = 1
  sleep[sleep$sleep_q2 > 30 & sleep$sleep_q2 <= 60, "sleep_q2_scored"] = 2
  sleep[sleep$sleep_q2 > 60, "sleep_q2_scored"] = 3
  
  sleep[!is.na(sleep$sleep_q4) & sleep$sleep_q4 > 7, "sleep_q4_scored"] = 0
  sleep[!is.na(sleep$sleep_q4) & sleep$sleep_q4 > 6 & sleep$sleep_q4 <= 7, "sleep_q4_scored"] = 1
  sleep[!is.na(sleep$sleep_q4) & sleep$sleep_q4 > 5 & sleep$sleep_q4 <= 6, "sleep_q4_scored"] = 2
  sleep[!is.na(sleep$sleep_q4) & sleep$sleep_q4 <= 5, "sleep_q4_scored"] = 3
  
  sleep$sleep_q1_recoded = sleep$sleep_q1
  sleep$sleep_latency = rowSums(sleep[,c("sleep_q2_scored", "sleep_q5a", "sleep_q9")])
  sleep[!is.na(sleep$sleep_latency) & sleep$sleep_latency ==0, "sleep_latency_scored"] = 0
  sleep[!is.na(sleep$sleep_latency) & sleep$sleep_latency >= 1 & sleep$sleep_latency < 3, "sleep_latency_scored"] = 1
  sleep[!is.na(sleep$sleep_latency) & sleep$sleep_latency >= 3 & sleep$sleep_latency < 5, "sleep_latency_scored"] = 2
  sleep[!is.na(sleep$sleep_latency) & sleep$sleep_latency >= 5, "sleep_latency_scored"] = 3
  
  sleep$timeinbed = difftime(sleep$sleep_q3, sleep$sleep_q1, units="hours")
  sleep[sleep$timeinbed < -8, "timeinbed"] = sleep[sleep$timeinbed < -8, "timeinbed"] + 24
  sleep[sleep$timeinbed < 0, "timeinbed"] = sleep[sleep$timeinbed < 0, "timeinbed"] + 12
  
  sleep$timeinbed = as.numeric(sleep$timeinbed)
  sleep$sleep_efficiency = sleep$sleep_q4 / sleep$timeinbed
  
  sleep[!is.na(sleep$sleep_efficiency) & sleep$sleep_efficiency >= .85, "sleep_efficiency_scored"] = 0
  sleep[!is.na(sleep$sleep_efficiency) & sleep$sleep_efficiency >= .75 & sleep$sleep_efficiency < .85, "sleep_efficiency_scored"] = 1
  sleep[!is.na(sleep$sleep_efficiency) & sleep$sleep_efficiency >= .65 & sleep$sleep_efficiency < .75, "sleep_efficiency_scored"] = 2
  sleep[!is.na(sleep$sleep_efficiency) & sleep$sleep_efficiency < .65, "sleep_efficiency_scored"] = 3
  
  sleep$sleep_disturbance = rowSums(sleep[,12:20], na.rm=T)
  
  sleep[sleep$sleep_disturbance ==0, "sleep_disturbance_scored"] = 0
  sleep[sleep$sleep_disturbance >= 1 & sleep$sleep_disturbance < 10, "sleep_disturbance_scored"] = 1
  sleep[sleep$sleep_disturbance >= 10 & sleep$sleep_disturbance < 19, "sleep_disturbance_scored"] = 2
  sleep[sleep$sleep_disturbance >= 19, "sleep_disturbance_scored"] = 3
  
  
  sleep$daytime_dysfunction = rowSums(sleep[,c("sleep_q7", "sleep_q8")])
  sleep[!is.na(sleep$daytime_dysfunction) & sleep$daytime_dysfunction < 2, "daytime_dysfunction_scored"] = 0
  sleep[!is.na(sleep$daytime_dysfunction) & sleep$daytime_dysfunction >= 2 & sleep$daytime_dysfunction < 4, "daytime_dysfunction_scored"] = 1
  sleep[!is.na(sleep$daytime_dysfunction) & sleep$daytime_dysfunction >= 4 & sleep$daytime_dysfunction < 6, "daytime_dysfunction_scored"] = 2
  sleep[!is.na(sleep$daytime_dysfunction) & sleep$daytime_dysfunction >= 6, "daytime_dysfunction_scored"] = 3
  
  sleep$pisq_score = rowSums(sleep[,c("sleep_latency_scored", "sleep_q4_scored", "sleep_q9", "sleep_efficiency_scored", "sleep_disturbance_scored", "daytime_dysfunction_scored")])
  
  
  sleep <- sleep %>%
    rename(pisq_timestamp = pit_sleep_quality_index_timestamp) %>%
    select(record_id, pisq_timestamp, sleep_q2_scored:pisq_score)
  
  return(sleep)
  
}


#' Process PRQC-SF data
#'
#' This function will download and compute total scores for the
#' Perceived Relationship Quality Component scale
#'
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name (default is 'infant_6months_arm_1')
#' @return A data frame for the completed surveys
#' @export
get_prqc <- function(token, timepoint='infant_6months_arm_1') {
  library(dplyr)
  prqc <- get_data(token, form='relationship_quality_prqcsf_covid')
  prqc <- filter(prqc, redcap_event_name == timepoint)
  
  prqc <- prqc %>%
    rename(prqc_satisfaction_precov = prqc_q01, prqc_satisfaction_now = prqc_q02,
           prqc_commitment_precov = prqc_q03, prqc_commitment_now = prqc_q04,
           prqc_intimacy_precov = prqc_q05, prqc_intimacy_now = prqc_q06,
           prqc_trust_precov = prqc_q07, prqc_trust_now = prqc_q08,
           prqc_passion_precov = prqc_q09, prqc_passion_now = prqc_q10,
           prqc_love_precov = prqc_q11, prqc_love_now = prqc_q12,
           prqc_special_effort_precov = prqc_q13, prqc_special_effort_now = prqc_q14)
  
  prqc <- prqc %>%
    rename(prqc_timestamp = relationship_quality_prqcsf_covid_timestamp) %>%
    select(record_id, prqc_timestamp, prqc_satisfaction_precov:prqc_special_effort_now)
  
  return(prqc)
}

#' Process Infant Care Questionnaire - REPLAACE
#'
#' This function will download and compute total scores for the
#' Infant Care Questionnaire scale
#'
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name (default is 'infant_6months_arm_1')
#' @return A data frame for the completed surveys
#' @export
get_infant_care <- function(token, timepoint='infant_6months_arm_1') {
  library(dplyr)
  icq <- get_data(token, form='infant_care_questionnaire')
  icq <- filter(icq, redcap_event_name == timepoint)
  
  #reverse scoring
  reversed <- c('icq_12', 'icq_15', 'icq_17', 'icq_18', 'icq_22')
  icq[reversed] <- lapply(icq[reversed], function(x) 6 - x)
  
  #d1: mom&baby
  d1_cols <- c('icq_1', 'icq_2', 'icq_3', 'icq_4', 'icq_5', 'icq_6', 'icq_7', 'icq_8', 'icq_9', 'icq_10',
               'icq_11', 'icq_12', 'icq_13', 'icq_14')
  icq$icq_mom_baby = rowMeans(icq[, d1_cols], na.rm=T)
  
  #d2: emotionality
  d2_cols = c('icq_15', 'icq_16', 'icq_17', 'icq_18')
  icq$icq_emotionality = rowMeans(icq[, d2_cols], na.rm=T)
  
  #d3: responsiveness
  d3_cols = c('icq_19', 'icq_20', 'icq_21', 'icq_22')
  icq$icq_responsiveness = rowMeans(icq[, d3_cols], na.rm=T)
  
  icq <- icq %>%
    rename(icq_timestamp = infant_care_questionnaire_timestamp) %>%
    select(record_id, icq_timestamp, icq_mom_baby, icq_emotionality, icq_responsiveness)
  
  return(icq)
}

#' Process CESD Depression Scores 
#'
#' This function will download and compute total scores for the
#' CESD scale
#'
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name (default is 'infant_12months2_arm_1')
#' @return A data frame for the completed surveys
#' @export
get_cesd <- function(token, timepoint = 'infant_12months2_arm_1') {
  library(dplyr)
  cesd <- get_data(token, form='center_for_epidemiologic_studies_depression_scale')
  cesd <- filter(cesd, redcap_event_name == timepoint)
  
  #4s need to be replaced with NAs (i don't know)
  cesd[cesd == 4] <- NA
  #items 4,8,12,16 need to be reverse scored
  reversed = c('cesd4', 'cesd8', 'cesd12', 'cesd16')
  cesd[reversed] <- lapply(cesd[reversed], function(x) 3 - x)
  
  cesd$cesd_score <- rowSums(cesd[, 7:26], na.rm=T)
  
  cesd <- cesd %>%
    rename(cesd_timestamp = center_for_epidemiologic_studies_depression_scale_timestamp) %>%
    select(record_id, cesd_timestamp, cesd_score)
  
  return(cesd)
  
}

#' Process Patient Health Questionnaire Scores 
#'
#' This function will download and compute total scores for the
#' PHQ scale
#'
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name (default is 'infant_9months_arm_1')
#' @return A data frame for the completed surveys
#' @export
get_phq <- function(token, timepoint='infant_9months_arm_1') {
  library(dplyr)
  phq <- get_data(token, form='patient_health_questionnaire_9')
  phq <- filter(phq, redcap_event_name == timepoint)
  
  cols <- c('phq9_1', 'phq9_2', 'phq9_3', 'phq9_4', 'phq9_5', 'phq9_6', 'phq9_7', 'phq9_8', 'phq9_9')
  phq$phq_score <- rowSums(phq[, cols], na.rm=T)
  
  phq <- phq %>%
    mutate(phq_severity = case_when(
      phq_score <= 4 ~ 1, phq_score >= 5 & phq_score <= 9 ~ 2, phq_score >= 10 & phq_score <= 14 ~ 3,
      phq_score >= 15 & phq_score <= 19 ~ 4, phq_score >= 20 & phq_score <= 27 ~ 4
    ))
  
  phq <- phq %>%
    rename(phq_difficulty = phq9_how_difficult, phq_timestamp = patient_health_questionnaire_9_timestamp) %>%
    select(record_id, phq_timestamp, phq_score, phq_difficulty, phq_severity)
  
  return(phq)
  
}

#' Process Early Executives Functions Questionnaire
#'
#' This function will download and compute total scores for the
#' EEFQ scale
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the completed surveys
#' @export
get_eefq <- function(token) {
  library(dplyr)
  eefq <- get_data(token, form='early_executive_function_q_survey')
  
  eefq[eefq == 8] <- NA
  
  reversed = c("matrix_3", "matrix_9", "matrix_11", "matrix_16", "matrix_22", "matrix_23", "matrix_24", "matrix_25", "matrix_26", "matrix_27", "matrix_28")
  eefq[reversed] <- lapply(eefq[reversed], function(x) 8 - x)
  
  ic <- c("matrix_1", "matrix_2", "matrix_3", "matrix_4", "matrix_5", "matrix_6", "matrix_7")
  fx <- c("matrix_8", "matrix_9", "matrix_10", "matrix_11", "matrix_12", "matrix_13", "matrix_14")
  wm <- c("matrix_15", "matrix_16", "matrix_17", "matrix_18", "matrix_19", "matrix_20")
  rg <- c("matrix_21", "matrix_22", "matrix_23", "matrix_24", "matrix_25", "matrix_26", "matrix_27", "matrix_28")
  
  eefq$eefq_ic <- rowMeans(eefq[, ic], na.rm=T)
  eefq$eefq_fx <- rowMeans(eefq[, fx], na.rm=T)
  eefq$eefq_wm <- rowMeans(eefq[, wm], na.rm=T)
  eefq$eefq_rg <- rowMeans(eefq[, rg], na.rm=T)
  
  eefq <- eefq %>%
    rename(eefq_timestamp = early_executive_function_q_survey_timestamp,
           eefq_ic_waiting_game = eefq_waiting_game, eefq_wm_finding_game = eefq_finding_game, eefq_fx_sorting_game = eefq_sorting_game) %>%
    select(record_id, eefq_timestamp, eefq_ic, eefq_fx, eefq_wm, eefq_rg, eefq_ic_waiting_game, eefq_fx_sorting_game, eefq_wm_finding_game)
  
  return(eefq)
}

#' Process Difficulties in Emotion Regulation Scores
#'
#' This function will download and compute total scores for the
#' DERS scale
#'
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name (default is 'infant_30months_arm_1')
#' @return A data frame for the completed surveys
#' @export
get_ders <- function(token, timepoint='infant_30months_arm_1') {
  library(dplyr)
  ders <- get_data(token, form='difficulties_in_emotional_regulation_ders16item')
  ders <- filter(ders, redcap_event_name == timepoint)
  
  ders$ders_non_accept <- rowSums(ders[, c('ders_9', 'ders_10', 'ders_13')], na.rm=T)
  ders$ders_goals <- rowSums(ders[, c('ders_3', 'ders_7', 'ders_15')], na.rm=T)
  ders$ders_impulse <- rowSums(ders[, c('ders_4', 'ders_8', 'ders_11')], na.rm=T)
  ders$ders_strategies <- rowSums(ders[, c('ders_5', 'ders_6', 'ders_12', 'ders_14', 'ders_16')], na.rm=T)
  ders$ders_clarity <- rowSums(ders[, c('ders_1', 'ders_2')], na.rm=T)
  
  ders <- ders %>%
    rename(ders_timestamp = difficulties_in_emotional_regulation_ders16item_timestamp) %>%
    select(record_id, ders_timestamp, ders_non_accept, ders_goals, ders_impulse, ders_strategies, ders_clarity)
  
  return(ders)
  
}

#' Process IOUS Scores
#'
#' This function will download and compute total scores for the
#' IOUS scale
#'
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name (default is 'infant_30months_arm_1')
#' @return A data frame for the completed surveys
#' @export
get_ious27 <- function(token, timepoint='infant_30months_arm_1') {
  library(dplyr)
  
  ious <- get_data(token, form='intolerance_of_uncertainty_scale_ius27item')
  ious <- filter(ious, redcap_event_name == timepoint)
  
  ious$ious27_score <- rowSums(ious[, 7:33], na.rm=T)
  
  ious <- ious %>%
    rename(ious27_timestamp = intolerance_of_uncertainty_scale_ius27item_timestamp) %>%
    select(record_id, ious27_timestamp, ious27_score)
  
  return(ious)
  
}

#' Process Beck Depression Scores 
#'
#' This function will download and compute total scores for the
#' Beck Depression Inventory 
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the completed surveys
#' @export
get_beck <- function(token) {
  library(dplyr)
  beck <- get_data(token, form='becks_depression_inventory')
  
  beck <- beck %>%
    rename(beck_timestamp = becks_depression_inventory_timestamp) %>%
    select(record_id, beck_timestamp, beck_score, beck_severity)
  
  return(beck)
}

#' Process State-Trait Anxiety Inventory Scores 
#'
#' This function will download and compute total scores for the
#' STAI
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the completed surveys
#' @export
get_stai <- function(token) {
  library(dplyr)
  stai <- get_data(token, form='state_trait_anxiety_inventory')
  
  reversed <- c('stai_tense', 'stai_strained', 'stai_upset', 'stai_misfortune', 'stai_frightened', 'stai_nervous', 'stai_jittery',
                'stai_indecisive', 'stai_worried', 'stai_confused', 'stai_nervrest', 'stai_failure', 'stai_difficultiespiling', 'stai_worrymatter',
                'stai_disturbingthoughts', 'stai_selfconfidence', 'stai_inadequate', 'stai_unimportantthoughts', 'stai_disappointment',
                'stai_tension')
  stais <- c('stai_calm', 'stai_secure', 'stai_tense', 'stai_strained', 'stai_ease', 'stai_upset', 'stai_misfortune', 'stai_satisfied',
             'stai_frightened', 'stai_comfortable', 'stai_selfconfident', 'stai_nervous', 'stai_jittery', 'stai_indecisive', 'stai_relaxed',
             'stai_feelcontent', 'stai_worried', 'stai_confused', 'stai_steady', 'stai_pleasant')
  
  stait <- c('stai_nervrest', 'stai_satisfiedself', 'stai_happyothers', 'stai_failure', 'stai_rested', 'stai_calmcool', 'stai_difficultiespiling',
             'stai_worrymatter', 'stai_happy', 'stai_disturbingthoughts', 'stai_selfconfidence', 'stai_secure2','stai_decisioneasily', 'stai_inadequate',
             'stai_amcontent', 'stai_unimportantthoughts', 'stai_disappointment', 'stai_steadyperson', 'stai_tension')
  
  stai[reversed] <- lapply(stai[reversed], function(x) 5 - x)
  
  stai$stai_s = rowSums(stai[stais], na.rm=T)
  stai$stai_t = rowSums(stai[stait], na.rm=T)
  
  stai <- stai %>%
    rename(stai_timestamp = state_trait_anxiety_inventory_timestamp) %>%
    select(record_id, stai_timestamp, stai_s, stai_t)
  
  return(stai)
  
}
