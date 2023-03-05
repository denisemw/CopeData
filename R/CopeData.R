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
#' @return A data frame for the completed surveys
#' @export
get_data <- function(token = token, form = form, raw_v_label = 'raw') {
  record_filter = paste("[", form, "_complete]=2", sep = "")
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
                   exportSurveyFields='false',
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

  chaos$total_score =  rowMeans(chaos[,c("chaos_late", "chaos_commotion","chaos_rushed", "chaos_interrupt", "chaos_plans",
                                         "chaos_arguement", "chaos_routine","chaos_findthings", "chaos_ontop",
                                         "chaos_zoo", "chaos_fuss", "chaos_think", "chaos_rushed", "chaos_relax", "chaos_calm")], na.rm=T)
  chaos = chaos[,c("record_id", "redcap_event_name", "total_score")]
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
  asq$comm = rowSums(asq[,5:10],na.rm=TRUE)
  asq$grossmotor = rowSums(asq[,11:15],na.rm=TRUE)
  asq$finemotor = rowSums(asq[,16:20],na.rm=TRUE)
  asq$probsolve = rowSums(asq[,21:25],na.rm=TRUE)
  asq$social = rowSums(asq[,26:30],na.rm=TRUE)
  asq = asq[,c("record_id", "comm", "grossmotor", "finemotor", "probsolve", "social")]
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
  pss = pss[,c("record_id", "pss_score", "pss_14_score")]
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
  
  epds = epds[,c("record_id", "epds_all", "epds_dep", "epds_anx")]
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
  thq <- thq[,c("record_id", "thq_events_number")]
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
  parent_reward = get_data(token, "parenting_reward_questionnaire")
  parent_reward =  dplyr::filter(parent_reward, redcap_event_name == timepoint)
  parent_reward$scale_reward = parent_reward$scale_reward/2
  parent_reward$parenting_reward_avg = rowMeans(parent_reward[, c("prq_matrix_q01","prq_matrix_q02","prq_matrix_q03","prq_matrix_q04",
                                                                  "prq_matrix_q05","prq_matrix_q07","prq_matrix_q08","prq_matrix_q09",
                                                                  "prq_matrix_q10","prq_matrix_q11","prq_matrix_q12","prq_matrix_q13",
                                                                  "prq_matrix_q14", "scale_reward")], na.rm=T)
  parent_reward = parent_reward[,c("record_id", "parenting_reward_avg")]
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
  reward = reward[,c("record_id", "reward_avg")]
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
  bitsea = bitsea[,c("record_id", "autism_competence", "autism_problems", "autism_total", "competence", "problems")]
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
  parent_stress = get_data("parenting_stress_index_fourth_edition_short_form_p")
  parent_stress = parent_stress %>% dplyr::filter(redcap_event_name == timepoint)
  parent_stress$psi = rowMeans(parent_stress[, 5:40])
  parent_stress = parent_stress[,c("record_id", "psi")]
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
  mpas = mpas[,c("record_id", "pleasure", "quality")]
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
  parent_strain = get_data(token, "parenting_stress_role_strain_covid")
  parent_strain =  dplyr::filter(parent_strain, redcap_event_name == timepoint)
  cols = c(1,6,8,10,12,14,16,18,20)
  parent_strain = parent_strain[,cols]
  parent_strain$strain = rowMeans(parent_strain[,2:9])
  parent_strain = parent_strain[,c("record_id", "strain")]
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
  ibq = ibq[,c("record_id", "ibq_sur", "ibq_neg", "ibq_ec")]
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
  baseline$income_binned[baseline$income == 16 | baseline$income == 6] <- 3
  baseline$income_binned[baseline$income == 7 | baseline$income == 8] <- 4
  baseline$income_binned[baseline$income == 9 | baseline$income == 10] <- 5
  baseline$income_binned[baseline$income == 11 | baseline$income == 12] <- 6
  baseline$income_binned[baseline$income == 13 | baseline$income == 14] <- 7
  baseline$income_binned[baseline$income == 15 | baseline$income == 16] <- 8
  baseline$income_binned[baseline$income == 4] <- 9

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


