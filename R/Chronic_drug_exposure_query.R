#' Get specific drug exposure entries for adherence calculations
#'
#'
#' @param connection Database connection based on individual connection details (ordinarily as conn-variable)
#' @param schema String of OMOP schema which includes the necessary drug_exposure table from which adherence can be calculated.
#' @param ingredients Specifies medication under observation. Uses RxNorm Ingredient concept code as acceptable parameter (Function converts drug_exposure drug_concept_id into specific ingredients which he medication consists of). By default the parameter uses ing_4_sql, which are so called mapped chronic medication ingredients but can be adjusted for preference
#' @param cohort_schema Possible include only certain people that exist in a specific defined cohort between a time period. Parameter cohort schema must be a string value of the database schema with the defined cohort.
#' @param cohort_id numeric value of the defined cohort that has included person ids as well as start and end date of the cohort.
#'
#'
#' @return Dataframe of drug_exposure entries which consists of necessary columns for adherence calculation
#' @export
#'
#' @examples chronic_drug_exposure(connection=conn, schema='ohdsi_cdm_202207')
#' @examples chronic_drug_exposure(connection=conn, schema='ohdsi', ingredients=c('1502905', '1580747,1503297'))
#' @examples chronic_drug_exposure(connection=conn, schema='ohdsi_cdm_202208', ingredients = '1322184')
#' @examples chronic_drug_exposure(connection=conn, schema='ohdsi_cdm_202207', cohort_schema = 'ohdsi_result_202207', cohort_id=577)
chronic_drug_exposure<-function(connection, schema, ingredients = ing_4_sql, cohort_schema = 0, cohort_id = 0){

  if (length(ingredients)>1){
    options(useFancyQuotes = FALSE)
    ingredients<-paste(sQuote(paste0('{',ingredients,'}')), collapse=", ")
      }
  else if(ingredients!=ing_4_sql){
    options(useFancyQuotes = FALSE)
    ingredients<-paste(sQuote(paste0('{',ingredients,'}')))
  }

  if (cohort_schema==0){

    query <- paste0("
  select
          de.person_id,
          de.drug_concept_id,
          de.drug_exposure_start_date,
          extract(YEAR from de.drug_exposure_start_date) as year,
          de.days_supply,
          de.visit_occurrence_id,
          ca.ingredients

  from ",schema,".drug_exposure de
  join ",schema,".concept c on de.drug_concept_id = c.concept_id

  join (
      select array_agg(ca.ancestor_concept_id order by ca.ancestor_concept_id desc) as ingredients,
             ca.descendant_concept_id as package
      from ",schema,".concept_ancestor ca
      join ",schema,".concept c on ca.ancestor_concept_id = c.concept_id
      where c.concept_class_id = 'Ingredient'
      group by ca.descendant_concept_id
  )
      ca on ca.package=de.drug_concept_id

  where c.concept_class_id like '%Drug%'
  and ca.ingredients in (",noquote(ingredients),")
  ;")

  df<-querySql(connection, query)
  return(df)
  }

  else{
    query <- paste0("
  select
          de.person_id,
          de.drug_concept_id,
          de.drug_exposure_start_date,
          extract(YEAR from de.drug_exposure_start_date) as year,
          de.days_supply,
          de.visit_occurrence_id,
          ca.ingredients

  from ",schema,".drug_exposure de
  join ",schema,".concept c on de.drug_concept_id = c.concept_id

  join (
      select array_agg(ca.ancestor_concept_id order by ca.ancestor_concept_id desc) as ingredients,
             ca.descendant_concept_id as package
      from ",schema,".concept_ancestor ca
      join ",schema,".concept c on ca.ancestor_concept_id = c.concept_id

      where c.concept_class_id = 'Ingredient'
      group by ca.descendant_concept_id
  )
      ca on ca.package=de.drug_concept_id
  join ", cohort_schema,".cohort co on co.subject_id=de.person_id
  where c.concept_class_id like '%Drug%'
  and ca.ingredients in (",noquote(ingredients),")
  and co.cohort_definition_id=",cohort_id,"
  and de.drug_exposure_start_date between co.cohort_start_date and co.cohort_end_date
  ;")

    df<-querySql(connection, query)
    return(df)

  }
}




