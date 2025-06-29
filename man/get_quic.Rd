% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CopeData.R
\name{get_quic}
\alias{get_quic}
\title{Process Questionnaire of Unpredictability}
\usage{
get_quic(token, all_items = F)
}
\arguments{
\item{token}{Unique REDCap token ID}

\item{all_items}{Boolean - whether to pull raw item scores as well as the subscales}
}
\value{
A data frame for the completed surveys
}
\description{
This function will download and compute total scores for the
QUIC
}
