#' Query to get chronic drug exposure drug events from OMOP drug_exposure table
#'
#' @param schema (string of OMOP schema, with chronic drug exposure)
#'
#' @return query for querysql
#' @export
#'
#' @examples chronic_drug_exposure_query('ohdsi_cdm_202207')
chronic_drug_exposure_query<-function(schema){
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
and ca.ingredients in (",noquote(ing_4_sql),")
;")

return(query)
}
