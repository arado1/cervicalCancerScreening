# In the R console, to create a new package, run:
install.packages("devtools")
library(devtools)

# Use your specific path to create the package
create_package("C:/Users/Amir Rad/Documents/Cervical Cancer Screening")

# Then, inside the package directory, create the R functions.
# Function to check cervical cancer screening recommendations
screening_recommendation <- function(age, cytology_result, hpv_result, genotype, history, hysterectomy = FALSE) {
  
  # Define the recommendations
  recommendation <- ""
  grade <- ""
  
  # Logic for women with hysterectomy
  if (hysterectomy) {
    recommendation <- "No screening needed (hysterectomy with cervix removed)."
    grade <- "D"
    return(list(recommendation = recommendation, grade = grade))
  }
  
  # Logic for women younger than 21
  if (age < 21) {
    recommendation <- "No screening"
    grade <- "D"
  } 
  
  # Logic for women ages 21 to 29
  else if (age >= 21 && age <= 29) {
    if (cytology_result == "Normal") {
      recommendation <- "Screening every 3 years with cytology"
      grade <- "A"
    } else {
      recommendation <- "Abnormal cytology - Refer for further testing"
      grade <- "A"
    }
  } 
  
  # Logic for women ages 30 to 65
  else if (age >= 30 && age <= 65) {
    if (hpv_result == "Negative") {
      recommendation <- "Screening every 5 years with HPV test"
      grade <- "A"
    } else if (hpv_result == "Positive") {
      if (genotype %in% c("16", "18")) {
        recommendation <- "High-risk HPV detected, refer for colposcopy"
        grade <- "A"
      } else {
        recommendation <- "HPV Positive, continue screening every 5 years with HPV or co-testing"
        grade <- "A"
      }
    } else {
      recommendation <- "Abnormal HPV result - Refer for further testing"
      grade <- "A"
    }
  }
  
  # Logic for women older than 65
  else if (age > 65) {
    if (history == "Adequate prior screening and no high risk") {
      recommendation <- "No screening needed"
      grade <- "D"
    } else {
      recommendation <- "High-risk history - refer for screening"
      grade <- "D"
    }
  }
  
  # Logic for women with hysterectomy
  if (history == "Prior hysterectomy with no cervix" && !grepl("CIN", history)) {
    recommendation <- "No screening needed"
    grade <- "D"
  }
  
  # Return the recommendation and grade
  return(list(recommendation = recommendation, grade = grade))
}

# Create the required folders
dir.create("C:/Users/Amir Rad/Documents/Cervical Cancer Screening/R")
dir.create("C:/Users/Amir Rad/Documents/Cervical Cancer Screening/man")
# Define the file path for the DESCRIPTION file
description_path <- "C:/Users/Amir Rad/Documents/Cervical Cancer Screening/DESCRIPTION"

# Create the DESCRIPTION file content
description_content <- "
Package: cervicalCancerScreening
Type: Package
Title: An R package for cervical cancer screening recommendations
Version: 0.1.0
Author: Amir Rad
Maintainer: Amir Rad <your.email@example.com>
Description: This package helps in providing cervical cancer screening recommendations based on test results and medical history, following the Norwegian guidelines.
License: What license it uses
Encoding: UTF-8
LazyData: true
Depends:
  R (>= 4.0)
"

# Write the content to the DESCRIPTION file
writeLines(description_content, description_path)

# Verify that the file has been created
file.exists(description_path)  # Should return TRUE if the file is created

# Define the file path for the NAMESPACE file
namespace_path <- "C:/Users/Amir Rad/Documents/Cervical Cancer Screening/NAMESPACE"

# Create the content for the NAMESPACE file
namespace_content <- "
# NAMESPACE
export(screening_recommendation)
"

# Write the content to the NAMESPACE file
writeLines(namespace_content, namespace_path)

# Verify that the file has been created
file.exists(namespace_path)  # Should return TRUE if the file is created
# Create the R folder if it doesn't exist
dir.create("C:/Users/Amir Rad/Documents/Cervical Cancer Screening/R", showWarnings = FALSE)

# Define the R function content
function_content <- "
#' @title Cervical Cancer Screening Recommendation
#' 
#' @description This function provides the cervical cancer screening recommendation based on age, cytology result, HPV test result, genotype, and medical history.
#' 
#' @param age Age of the woman.
#' @param cytology_result Cytology test result (e.g., 'Normal', 'Abnormal').
#' @param hpv_result HPV test result (e.g., 'Positive', 'Negative').
#' @param genotype HPV genotype (e.g., '16', '18', 'Other').
#' @param history Medical history (e.g., 'None', 'CIN2', 'CIN3', 'Prior hysterectomy').
#' @param hysterectomy Whether the woman has had a hysterectomy (default is FALSE).
#' 
#' @return A list containing the screening recommendation and its grade.
#' 
#' @examples
#' screening_recommendation(age = 28, cytology_result = 'Normal', hpv_result = 'Negative', genotype = 'None', history = 'None')
#' 
#' @export
screening_recommendation <- function(age, cytology_result, hpv_result, genotype, history, hysterectomy = FALSE) {
  # Define recommendation
  recommendation <- ''
  grade <- ''
  
  # Logic for women with hysterectomy
  if (hysterectomy) {
    recommendation <- 'No screening needed (hysterectomy with cervix removed).'
    grade <- 'D'
    return(list(recommendation = recommendation, grade = grade))
  }
  
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
"

# Save the function in the R folder
writeLines(function_content, "C:/Users/Amir Rad/Documents/Cervical Cancer Screening/R/screening_recommendation.R")


# Load the devtools package
library(devtools)

# Generate the documentation
devtools::document("C:/Users/Amir Rad/Documents/Cervical Cancer Screening")

# Install the package
devtools::install("C:/Users/Amir Rad/Documents/Cervical Cancer Screening")


# Example of using the function
result <- screening_recommendation(age = 28, cytology_result = "Normal", hpv_result = "Negative", genotype = "None", history = "None")

print(result$recommendation)  # "Screening every 3 years with cytology"
print(result$grade)          # "A"
