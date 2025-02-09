% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/screening_recommendation.R
\name{screening_recommendation}
\alias{screening_recommendation}
\title{Cervical Cancer Screening Recommendation}
\usage{
screening_recommendation(
  age,
  cytology_result,
  hpv_result,
  genotype,
  history,
  hysterectomy = FALSE
)
}
\arguments{
\item{age}{Age of the woman.}

\item{cytology_result}{Cytology test result (e.g., 'Normal', 'Abnormal').}

\item{hpv_result}{HPV test result (e.g., 'Positive', 'Negative').}

\item{genotype}{HPV genotype (e.g., '16', '18', 'Other').}

\item{history}{Medical history (e.g., 'None', 'CIN2', 'CIN3', 'Prior hysterectomy').}

\item{hysterectomy}{Whether the woman has had a hysterectomy (default is FALSE).}
}
\value{
A list containing the screening recommendation and its grade.
}
\description{
This function provides the cervical cancer screening recommendation based on age, cytology result, HPV test result, genotype, and medical history.
}
\examples{
screening_recommendation(age = 28, cytology_result = 'Normal', hpv_result = 'Negative', genotype = 'None', history = 'None')

}
