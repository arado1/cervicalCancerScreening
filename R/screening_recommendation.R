
#' @title Cervical Cancer Screening Recommendation
#' 
#' @description This function provides the cervical cancer screening recommendation based on age, cytology result, HPV test result, genotype, and medical history.
#' 
#' @param age Age of the woman.
#' @param cytology_result Cytology test result (e.g., 'Normal', 'Abnormal').
#' @param hpv_result HPV test result (e.g., 'Positive', 'Negative').
#' @param genotype HPV genotype (e.g., '16', '18', 'Other').
#' @param history Medical history (e.g., 'None', 'CIN2', 'CIN3', 'Prior hysterectomy').
#' 
#' @return A list containing the screening recommendation and its grade.
#' 
#' @examples
#' screening_recommendation(age = 28, cytology_result = 'Normal', hpv_result = 'Negative', genotype = 'None', history = 'None')
#' 
#' @export
screening_recommendation <- function(age, cytology_result, hpv_result, genotype, history) {
  # Define recommendation
  recommendation <- ''
  grade <- ''
  
  # Logic for women younger than 21
  if (age < 21) {
    recommendation <- 'No screening'
    grade <- 'D'
  }
  
  # Logic for women ages 21 to 29
  else if (age >= 21 && age <= 29) {
    if (cytology_result == 'Normal') {
      recommendation <- 'Screening every 3 years with cytology'
      grade <- 'A'
    } else {
      recommendation <- 'Abnormal cytology - Refer for further testing'
      grade <- 'A'
    }
  }
  
  # Logic for women ages 30 to 65
  else if (age >= 30 && age <= 65) {
    if (hpv_result == 'Negative') {
      recommendation <- 'Screening every 5 years with HPV test'
      grade <- 'A'
    } else if (hpv_result == 'Positive') {
      if (genotype %in% c('16', '18')) {
        recommendation <- 'High-risk HPV detected, refer for colposcopy'
        grade <- 'A'
      } else {
        recommendation <- 'HPV Positive, continue screening every 5 years with HPV or co-testing'
        grade <- 'A'
      }
    } else {
      recommendation <- 'Abnormal HPV result - Refer for further testing'
      grade <- 'A'
    }
  }
  
  # Logic for women older than 65
  else if (age > 65) {
    if (history == 'Adequate prior screening and no high risk') {
      recommendation <- 'No screening needed'
      grade <- 'D'
    } else {
      recommendation <- 'High-risk history - refer for screening'
      grade <- 'D'
    }
  }
  
  # Logic for women with hysterectomy
  if (history == 'Prior hysterectomy with no cervix' && !grepl('CIN', history)) {
    recommendation <- 'No screening needed'
    grade <- 'D'
  }
  
  # Return the recommendation and grade
  return(list(recommendation = recommendation, grade = grade))
}

