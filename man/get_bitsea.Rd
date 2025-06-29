% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CopeData.R
\name{get_bitsea}
\alias{get_bitsea}
\title{Process 12 month BITSEA data}
\usage{
get_bitsea(token, timepoint = "infant_12months2_arm_1", all_items = F)
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
This function will download and return total scores
and cut off values for the competence, problems,
and autism subscales of the BITSEA survey at 12 months.
}
