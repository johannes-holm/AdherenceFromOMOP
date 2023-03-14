#' Calculate adherence for chronic medication takers from OMOP CDM
#'
#' @param df  - This should be the table from OMOP CDM drug_exposure with chronic medication events
#' @param person_id - By default the function calculates adherence for all people in the OMOP drug_exposure, who, by definition, are chronic medication consumers. If specified the function calculates adherence for the specific person in the the dataframe. Data type numeric (example: person_id=3)
#' @param cma - By default the function calculates adherence with all different CMA algorithms (CMA1-CMA9). If specified the function can calculate adherence using only necessary algorithm. Data type string (example: cma = 'CMA4' or cma = c('CMA1', 'CMA2'))
#' @param med - By default the function calculates adherence for all chronic medications (chronic medication ingredients). If specified the function can calculate adherence only for specific medication (chronic medication ingredient). Data type string (example: med='1332418'(RxNorm Ingredient id for Amlodipine) or med='19097684,1549786'(RxNorm Ingredient id for gestodene and ethinyl estradiol used in the same medication))
#'
#' @return A a list of dataframes (CMA1-CMA9) with included chronic medication consumers and their adherence to a specific medication (chronic medication ingredients). Adherence is calculated from the first event to the last with yearly calculations (first year, second year and so on). If adherence is not consistent with a year-long gap between years, function considers next adherence-year as the first again. To access a certain CMA's adherence dataframe, user has to refer the the specific CMA (example: calculated_list_of_dataframes$CMA5)
#' @export
#'
#' @examples adherence_function(table_from_chronic_drug_exposure_query, person_id=4, cma = c('CMA1','CMA2','CMA3','CMA4'), med = 1506270) - finds yearly adherences for the consumer with id 4 only for medication ingredient 1506270 (RxNorm Ingredient code for methinylprednisolone) with the help of CMA1-CMA4 algorithms.
adherence_function <-function(df, person_id = 0, cma = c('CMA1','CMA2','CMA3','CMA4','CMA5','CMA6','CMA7','CMA9'), med = 0) {

  df$DAYS_SUPPLY <- case_when(df$DAYS_SUPPLY <= 0 ~ 1,
                              TRUE ~ as.numeric(df$DAYS_SUPPLY))

  observed_years <- as.numeric(format(as.Date(max(df$DRUG_EXPOSURE_START_DATE)), "%Y")) - as.numeric(format(as.Date(min(df$DRUG_EXPOSURE_START_DATE)), "%Y"))

  if (med == 0) {
    if (person_id != 0) {
      person <- df %>% filter(PERSON_ID == person_id)

      person$INGREDIENTS<-gsub('[{}]', "", person$INGREDIENTS)
      ing<-gsub('[{}]', "",c(unique(chronic_med_ing$INGREDIENTS)))
      medications<-c(unique(chronic_med_ing$INGREDIENTS))
      names(medications)=sprintf("(INGREDIENTS == '%s')",ing )
      medications <-setNames(names(medications), medications)

      datalist <- vector('list')

      for (i in cma) {
        cma_med <- CMA_sliding_window(
          CMA.to.apply = i,
          data = person,
          ID.colname = "PERSON_ID",
          event.date.colname = "DRUG_EXPOSURE_START_DATE",
          event.duration.colname = "DAYS_SUPPLY",
          medication.class.colname = "INGREDIENTS",
          medication.groups =medications,
          followup.window.start = 0,
          followup.window.duration = 365 * observed_years,
          observation.window.start = 0,
          observation.window.duration = 365 * observed_years,
          sliding.window.start = 0,
          sliding.window.start.unit = "days",
          sliding.window.duration = 365,
          sliding.window.duration.unit = "days",
          sliding.window.step.duration = 365,
          sliding.window.step.unit = "days",
          date.format = "%Y-%m-%d"
        )
        test <- do.call(rbind, cma_med$CMA)
        test <- cbind(med_ingredients = rownames(test), test)
        rownames(test) <- 1:nrow(test)
        test$med_ingredients <- sub("\\.\\d+$", "", test$med_ingredients)
        dat <- test %>% mutate(ind = cumsum(is.na(CMA))) %>% filter(!is.na(CMA)) %>% group_by(ind,PERSON_ID, med_ingredients) %>% mutate(window.ID = 1:n())
        datalist[[i]] <- dat



        print(paste0('Calculated CMA: ', i))

      }
      return(datalist)
    }
    else{
      df$INGREDIENTS<-gsub('[{}]', "", df$INGREDIENTS)
      ing<-gsub('[{}]', "",c(unique(chronic_med_ing$INGREDIENTS)))
      medications<-c(unique(chronic_med_ing$INGREDIENTS))
      names(medications)=sprintf("(INGREDIENTS == '%s')",ing )
      medications <-setNames(names(medications), medications)

      datalist <- vector('list')

      for (i in cma) {
        cma_med <- CMA_sliding_window(
          CMA.to.apply = i,
          data = df,
          ID.colname = "PERSON_ID",
          event.date.colname = "DRUG_EXPOSURE_START_DATE",
          event.duration.colname = "DAYS_SUPPLY",
          medication.class.colname = "INGREDIENTS",
          medication.groups =medications,
          followup.window.start = 0,
          followup.window.duration = 365 * observed_years,
          observation.window.start = 0,
          observation.window.duration = 365 * observed_years,
          sliding.window.start = 0,
          sliding.window.start.unit = "days",
          sliding.window.duration = 365,
          sliding.window.duration.unit = "days",
          sliding.window.step.duration = 365,
          sliding.window.step.unit = "days",
          date.format = "%Y-%m-%d"
        )

        test <- do.call(rbind, cma_med$CMA)
        test <- cbind(med_ingredients = rownames(test), test)
        rownames(test) <- 1:nrow(test)
        test$med_ingredients <- sub("\\.\\d+$", "", test$med_ingredients)
        dat <- test %>% mutate(ind = cumsum(is.na(CMA))) %>% filter(!is.na(CMA)) %>% group_by(ind,PERSON_ID, med_ingredients) %>% mutate(window.ID = 1:n())
        datalist[[i]] <- dat

        print(paste0('Calculated CMA: ', i))

      }

      return(datalist)
    }

  }
  else{
    med<-paste0('{', med,'}')

    if (person_id != 0) {
      person <- df %>% filter(PERSON_ID == person_id)
      if (med %in% unique(person$INGREDIENTS)) {
        person_med <- person %>% filter(INGREDIENTS == med)

        datalist <- vector('list')

        for (i in cma) {
          cma_med <- CMA_sliding_window(
            CMA.to.apply = i,
            data = person_med,
            ID.colname = "PERSON_ID",
            event.date.colname = "DRUG_EXPOSURE_START_DATE",
            event.duration.colname = "DAYS_SUPPLY",
            medication.class.colname = "INGREDIENTS",
            followup.window.start = 0,
            followup.window.duration = 365 * observed_years,
            observation.window.start = 0,
            observation.window.duration = 365 * observed_years,
            sliding.window.start = 0,
            sliding.window.start.unit = "days",
            sliding.window.duration = 365,
            sliding.window.duration.unit = "days",
            sliding.window.step.duration = 365,
            sliding.window.step.unit = "days",
            date.format = "%Y-%m-%d"
          )

          cma_med$CMA <-cma_med$CMA %>% mutate(ind = cumsum(is.na(CMA))) %>% filter(!is.na(CMA)) %>% group_by(ind,PERSON_ID) %>% mutate(window.ID = 1:n())
          datalist[[i]] <- cma_med$CMA

          print(paste0('Calculated CMA: ', i))
        }

        return(datalist)
      }
      else{
        print(paste('Specified person',person_id ,'does not take specified medication:', med))
      }
    }

    else{
      if (med %in% unique(df$INGREDIENTS)) {
        table_med <- df %>% filter(INGREDIENTS == med)

        datalist <- vector("list")
        for (i in cma) {
          cma_med <- CMA_sliding_window(
            CMA.to.apply = i,
            data = table_med,
            ID.colname = "PERSON_ID",
            event.date.colname = "DRUG_EXPOSURE_START_DATE",
            event.duration.colname = "DAYS_SUPPLY",
            medication.class.colname = "INGREDIENTS",
            followup.window.start = 0,
            followup.window.duration = 365 * observed_years,
            observation.window.start = 0,
            observation.window.duration = 365 * observed_years,
            sliding.window.start = 0,
            sliding.window.start.unit = "days",
            sliding.window.duration = 365,
            sliding.window.duration.unit = "days",
            sliding.window.step.duration = 365,
            sliding.window.step.unit = "days",
            date.format = "%Y-%m-%d"
          )

          cma_med$CMA <-cma_med$CMA %>% mutate(ind = cumsum(is.na(CMA))) %>% filter(!is.na(CMA)) %>% group_by(ind,PERSON_ID) %>% mutate(window.ID = 1:n())
          datalist[[i]] <- cma_med$CMA

          print(paste0('Calculated CMA: ', i))
        }
        return(datalist)
      }
      else{
        print(paste('Specified medication',med,'is not included in the dataframe.'))
      }


    }
  }


}
