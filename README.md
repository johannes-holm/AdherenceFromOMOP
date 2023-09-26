
# Adherence from OMOP

Calculate adherence from OMOP CDM

## Description
Adherence is a concept that indicates how well does the patient follow the healthcare provider's recommendation. Medication adherence specifically shows if the patient follows their medication regimen or are there any gaps. 
This is important for medications which are meant to be consumed for chronic diseases, where the efficacy of the treatment is closely correlated with the continuity of medication consumption. One possibility to measure adherence is secondary database analysis. 
OMOP common data model is a standardized model, which provides a clear structure for healthcare data. It is possible to use OMOP CDM drug_exposure data (which consists of purchased medication records) to measure chronic medication adherence.
This package makes it easy to filter records where the subject has purchased chronic medication, calculate the adherence for said people grouped by medication and used year and visualize it with an interactive dashboard.

Package contains four main functions with additional data with medications which are meant to be used for chronic diseases. Before using the functions, connection with the OMOP database needs to be established, using third party package DatabaseConnector.
Necessary parameters for functions are documented under manuals folder (man)

1st function - Chronic_drug_exposure
  -helps to filter out prescription records from OMOP database (drug_exposure table) where the purchased medication is meant to be used against chronic disease. information about chronic medication is provided in the package under data folder. The medication has been mapped by 
  a healthcare professional but is not yet complete. 
  
2nd function - adherence_function
  -Main function that calculates adherence based on prescription records. Function uses third party package called AdhereR and returns a table with adherence results grouped by individual patients, their medications and years of usage.
  
3rd function - add_info_to_adherence 
  -Necessary function to add information about people (age and gender) and medications (ATC code) to the calcualted adherence table. Helps the analysis process and is necessary for the next interactive dashboard function.
  
4th function - Adherence_dashboard
  -Interactive shiny dashboard which helps to analyse the adherence of the population


