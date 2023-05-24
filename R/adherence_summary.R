
#' Summarize the adherence table for convenient analysis and optimized dashboard generation
#'
#' @param df_w_info This should be the calculated adherence table (done with function 'adherence_function') with the additional information about the patients (age, gender) as well as information about the purchased medication (ATC code for easy filtering). The information should be added to the adherence table with the funcyion 'add_info_to_adherence'.
#'
#' @return Function returns a summarized table with all possible combinations of gender, age-groups, medications and time window groupings. Summarized information includes information about average, median, count, maxiumum, minimum and quantiles of the CMAs.
#' @export
#'
#' @examples summarized_table<-adherence_summary(CMA6_w_information)
adherence_summary<- function(df_w_info){

  columns2<-c('years_taken', 'Age_group', 'Window_id', 'ATC', 'Gender', 'avg', 'median','count' , 'max', 'min', 'q25','q50','q75')
  atc_levels<-c(1,3,4,5,7)

  sum_table<-data.frame(matrix(ncol = length(columns2)))
  colnames(sum_table)=columns2

  years<-sort(unique(df_w_info$window.ID))



  #kõik muutujad (sugu, vanus, ravim ja aken)

  for (year in years){
    chosen_years<-df_w_info%>%filter(window.ID==year)%>%select(PERSON_ID, med_ingredients)
    tabel1<-df_w_info[(df_w_info$PERSON_ID %in% chosen_years$PERSON_ID)&(df_w_info$med_ingredients %in% chosen_years$med_ingredients),]

    #sugu üksi
    mm<-tabel1%>%group_by(PERSON_ID, gen=gender)%>%summarise(avg=mean(CMA))%>%group_by(gen)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
    sum_table<-sum_table%>%add_row(Gender=mm$gen, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

    #vanus üksi
    mm<-tabel1%>%group_by(PERSON_ID, age=AgeGroup)%>%summarise(avg=mean(CMA))%>%group_by(age)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
    sum_table<-sum_table%>%add_row(Age_group=mm$age, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

    #aken üksi
    mm<-tabel1%>%group_by(PERSON_ID, wdw=window.ID)%>%summarise(avg=mean(CMA))%>%group_by(wdw)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
    sum_table<-sum_table%>%add_row(Window_id=mm$wdw, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

    #sugu ja vanus
    mm<-tabel1%>%group_by(PERSON_ID, age=AgeGroup, gen = gender)%>%summarise(avg=mean(CMA))%>%group_by(age, gen)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
    sum_table<-sum_table%>%add_row(Age_group=mm$age, Gender=mm$gen, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

    #sugu ja aken
    mm<-tabel1%>%group_by(PERSON_ID, wdw=window.ID, gen = gender)%>%summarise(avg=mean(CMA))%>%group_by(wdw, gen)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
    sum_table<-sum_table%>%add_row(Window_id=mm$wdw, Gender=mm$gen, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

    #vanus ja aken
    mm<-tabel1%>%group_by(PERSON_ID, wdw=window.ID, age = AgeGroup)%>%summarise(avg=mean(CMA))%>%group_by(wdw, age)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
    sum_table<-sum_table%>%add_row(Window_id=mm$wdw, Age_group=mm$age, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

    #sugu, vanus ja aken
    mm<-tabel1%>%group_by(PERSON_ID, wdw=window.ID, age = AgeGroup, gen = gender)%>%summarise(avg=mean(CMA))%>%group_by(wdw, age, gen)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
    sum_table<-sum_table%>%add_row(Window_id=mm$wdw, Age_group=mm$age , Gender=mm$gen,  avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

    for (i in atc_levels){
      #kõik atribuudid
      mm<-tabel1%>%group_by(PERSON_ID, gen=gender, age=AgeGroup, med=substr(ATC_CODE,1,i), wdw=window.ID)%>%summarise(avg=mean(CMA))%>%group_by(age, gen, med, wdw)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
      sum_table<-sum_table%>%add_row(Age_group = mm$age, Gender=mm$gen, ATC=mm$med, Window_id=mm$wdw, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

      #ravim üksi
      mm<-tabel1%>%group_by(PERSON_ID, med=substr(ATC_CODE,1,i))%>%summarise(avg=mean(CMA))%>%group_by(med)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
      sum_table<-sum_table%>%add_row(ATC=mm$med, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

      #sugu ja ravim
      mm<-tabel1%>%group_by(PERSON_ID, med=substr(ATC_CODE,1,i), gen = gender)%>%summarise(avg=mean(CMA))%>%group_by(med, gen)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
      sum_table<-sum_table%>%add_row(ATC=mm$med, Gender=mm$gen, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

      #vanus ja ravim
      mm<-tabel1%>%group_by(PERSON_ID, med=substr(ATC_CODE,1,i), age = AgeGroup)%>%summarise(avg=mean(CMA))%>%group_by(med, age)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
      sum_table<-sum_table%>%add_row(ATC=mm$med, Age_group=mm$age, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

      #aken ja ravim
      mm<-tabel1%>%group_by(PERSON_ID, med=substr(ATC_CODE,1,i), wdw = window.ID)%>%summarise(avg=mean(CMA))%>%group_by(med, wdw)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
      sum_table<-sum_table%>%add_row(ATC=mm$med, Window_id=mm$wdw, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

      #sugu, vanus ja ravim
      mm<-tabel1%>%group_by(PERSON_ID, med=substr(ATC_CODE,1,i), gen = gender, age=AgeGroup)%>%summarise(avg=mean(CMA))%>%group_by(med, gen, age)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
      sum_table<-sum_table%>%add_row(ATC=mm$med, Gender=mm$gen, Age_group=mm$age, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

      #sugu, aken ja ravim
      mm<-tabel1%>%group_by(PERSON_ID, med=substr(ATC_CODE,1,i), gen = gender, wdw=window.ID)%>%summarise(avg=mean(CMA))%>%group_by(med, gen, wdw)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
      sum_table<-sum_table%>%add_row(ATC=mm$med, Gender=mm$gen, Window_id=mm$wdw, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

      #vanus, aken ja ravim
      mm<-tabel1%>%group_by(PERSON_ID, med=substr(ATC_CODE,1,i), age = AgeGroup, wdw=window.ID)%>%summarise(avg=mean(CMA))%>%group_by(med, age, wdw)%>%summarise(ave=mean(avg), medi=median(avg), maxi=max(avg), mini=min(avg), q25i = quantile(avg, 0.25),q50i = quantile(avg, 0.5),q75i = quantile(avg, 0.75), cnt=length(avg))
      sum_table<-sum_table%>%add_row(ATC=mm$med, Age_group=mm$age, Window_id=mm$wdw, avg=mm$ave, years_taken=year, median=mm$medi, max=mm$maxi, min=mm$mini, q25=mm$q25i, q50=mm$q50i, q75=mm$q75i, count=mm$cnt)

      }

  }

  sum_table<-sum_table%>%filter(!is.na(avg))

  sum_table$Age_group<-as.character(sum_table$Age_group)
  sum_table$q25<-as.numeric(sum_table$q25)
  sum_table$q50<-as.numeric(sum_table$q50)
  sum_table$q75<-as.numeric(sum_table$q75)

  sum_table<-sum_table%>%filter(count>5)

  sum_table[is.na(sum_table)]<-0

  return(sum_table)
}








