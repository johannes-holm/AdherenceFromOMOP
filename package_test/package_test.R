#----------------------------load necessary packages----------------------------
library(AdherenceFromOMOP)
library(DatabaseConnector)
library(tidyverse)


#---------------------------Appoint DB credentials------------------------------
readRenviron("C:/Users/user/.Renviron") #path to R environment
DATABASE<-paste(Sys.getenv("DB_HOST"),"/",Sys.getenv("DB_NAME"),sep='')
DB_USER<-Sys.getenv('DB_USERNAME')
DB_PASSWORD<-Sys.getenv('DB_PASSWORD')
DB_PORT<-Sys.getenv('DB_PORT')


#-----------------Create DB connection details with credentials-----------------
connectionDetails <- createConnectionDetails(
  dbms="postgresql",
  server=DATABASE,
  user=DB_USER,
  password=DB_PASSWORD,
  port=DB_PORT,
  pathToDriver = "C:/Users/user/Documents/R/win-library/4.1/DatabaseConnector/java"
)


#-------------------------Establish database connection-------------------------
conn <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::connect(connectionDetails)


#-First function: chronic medication prescription from OMOP drug_exposure table-
#--------Possible to add a defined cohort for the function----------------------

df<-chronic_drug_exposure(conn, 'ohdsi_cdm_schema')
df_100<-df%>%filter(PERSON_ID %in% c(1:100)) #For example purposes a smaller dataframe is used to calculate adherence



#----Calculate adherence based on prescription data from drug_exposure table----
cma_test<-adherence_function(df_100)


#---------------For test purposes we use CMA1 of the small df ------------------
cma1 <-cma_test$CMA1


#------------Add information to the calcualted adherence table------------------
cma1_w_info<-add_info_to_adherence(conn, cma1, 'ohdsi_cdm_schema')


#------------------Interactive dashboard for adherence--------------------------
adherence_dashboard(cma1_w_info)#

#------------------Summarized adherence generation------------------------------
summarized_table<-adherence_summary(cma1_w_info)

#----------------------Updated dashboard with summarized info-------------------
adherence_dashboard_summarized(summarized_table)
