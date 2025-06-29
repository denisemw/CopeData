% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CopeData.R
\name{get_reward}
\alias{get_reward}
\title{Process Reward Responsivity data}
\usage{
get_reward(token, timepoint = "infant_3months_arm_1", all_items = F)
}
\arguments{
\item{token}{Unique REDCap token ID}

\item{timepoint}{Survey timepoint branch requested}

\item{all_items}{Boolean - whether to pull raw item scores as well as the subscales}
}
\value{
A data frame for the completed surveys
}
\description{
This function will download and return mother's
average reward responsivity scores
}
