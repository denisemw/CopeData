% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CopeData.R
\name{get_pisq}
\alias{get_pisq}
\title{Process PSQI data}
\usage{
get_pisq(token, timepoint = "infant_6months_arm_1", all_items = F)
}
\arguments{
\item{token}{Unique REDCap token ID}

\item{timepoint}{redcap event name (default is 'infant_6months_arm_1')}

\item{all_items}{Boolean - whether to pull raw item scores as well as the subscales}
}
\value{
A data frame for the completed surveys
}
\description{
This function will download and compute total scores for the
Pit Sleep Quality Index scale
}
