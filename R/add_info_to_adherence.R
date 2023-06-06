#' Add additional info of chronic medication consumers to your calculated adherence dataframe
#'
#' @param connection Database connection based on individual connection details (ordinarily as conn-variable)
#' @param adherence_df calculated adherence dataframe using adherence function
#' @param schema OMOP CDM schema where info about the consumer is
#'
#' @return dataframe of medication adherence with included gender, age, year of birth, age group and medication's ATC code
#' @export
#'
#' @examples add_info_to_adherence(list_of_calculated_adherence_dataframes$CMA4, 'ohdsi_cdm_202207')
add_info_to_adherence <- function(connection, adherence_df, schema){
  id_age_gender<-querySql(connection, paste0("SELECT person_id, gender_concept_id, year_of_birth FROM ",schema,".person"))

  labs <- c(paste(seq(0, 79, by = 20), seq(0 + 20 - 1, 80 - 1, by = 20),sep = "-"), paste(80, "+", sep = ""))
  df_w_info<-
    merge(adherence_df, id_age_gender, by="PERSON_ID")%>%
    filter(GENDER_CONCEPT_ID !=0)
    mutate(gender = case_when(GENDER_CONCEPT_ID==8532 ~ "F",
                              GENDER_CONCEPT_ID==8507 ~ "M"))%>%
    mutate(age = as.numeric(format(window.start,"%Y")) - YEAR_OF_BIRTH)%>%
    mutate(AgeGroup = cut(age, breaks = c(seq(0, 80, by = 20), Inf), labels = labs, right = FALSE))
  df_w_info<-merge(x=df_w_info, y=chronic_med_ing, by.x="med_ingredients", by.y="INGREDIENTS")

  return(df_w_info)

}
