#' Dashboard of chronic medication adherence
#'
#' @param calculated_adherence_w_info This is supposed to be previously calculated CMAs of chronic medication consumers and their medication included with additional information of consumers (age, age group, gender) and additional information of medication (ATC code)
#'
#' @return returns a Shiny app with the possibilty to observe changes in medication adherence between genders, age groups and specific medication groups. The app has three main plots. First one shows medication adherence changes through time between male and female consumers. Second one shows total average medication adherence between different medication groups. Third one shows chenges in medication adherence between age groups. The app makes it possible to filter out specific genders and ages, as well as  filter out medication based on five different ATC levels (you may want to include only medication with ATC I level "A" or get more detailed with ATC II-V level for example A06AH04 - Naloxone).
#' @export
#'
#' @examples adherence_dashboard(calculated_adherence_with_additional_info_added_with_add_info_to_adherence_function)
adherence_dashboard<-function(calculated_adherence_w_info){
  total<- calculated_adherence_w_info

  n_fun <- function(x){
    return(data.frame(y = median(x) , label = paste0("n=",length(x), "\n",
                                                     "avg=", round(mean(x),4),"\n",
                                                     "m=", round(median(x),4), "\n"

    )))
  }


  ui <- fluidPage(

    titlePanel("Adherence"),

    fixedRow(

      column(
        3,

        sliderInput(
          inputId = "age_slider",
          label = "Age of patients",
          min = min(total$age),
          max = max(total$age),
          value = c(min(total$age), max(total$age))
        ),
        dropdownButton(
          label = "Medications",
          status = "default",
          width = "100%",
          circle = F,
          actionButton(
            inputId = "a2z",
            label = "Sort A to Z",
            icon = icon("sort-alpha-asc")
          ),
          actionButton(
            inputId = "z2a",
            label = "Sort Z to A",
            icon = icon("sort-alpha-desc")
          ),
          br(),
          actionButton(inputId = "all", label = "(Un)select all"),
          checkboxGroupInput(
            inputId = "check2",
            label = "Choose",
            choices = unique(substr(total$ATC_CODE, 1, 1)),
            selected = unique(substr(total$ATC_CODE, 1, 1))
          )
        )
      ),
      column(
        3,
        checkboxGroupInput(
          "gender",
          label = "Observed gender",
          choices = c("Male" = 'M', "Female"='F'),
          selected = c('M', 'F')
        ),
      ),
      column(
        3,
        radioButtons(
          "atc_level",
          label = "Observed ATC level",
          choices = list(
            "I level" = 1,
            "II level" = 3,
            "III level" = 4,
            "IV level" = 5,
            "V level" = 7
          ),
          selected = 1
        )
      ),
      column(
        3,
        radioButtons(
          "how_long",
          label = "How many years have people taken the medication (minimum)?",
          choices = sapply(list(unique(total$window.ID)), sort),

          selected = sapply(list(unique(total$window.ID)), sort)[1]
        )
      ),
    ),

    fluidRow(
      column(5,plotlyOutput("gender_plot"), offset=3)),
    fluidRow(
      column(5,plotOutput("plot2"), offset=3)),
    fluidRow(
      column(5,plotlyOutput("plot3"), offset=3)),
    fluidRow(
      column(5, plotOutput('adherence_change_gender'), offset = 3)),
    fluidRow(
      column(5, plotOutput('adherence_change_ages'), offset = 3))
  )

  server <- function(input, output, session) {
    digit_counts <- reactive({
      valitud_aastad <-total%>%filter(window.ID==input$how_long)%>%select(PERSON_ID, med_ingredients)
      total[(total$PERSON_ID %in% valitud_aastad$PERSON_ID)&(total$med_ingredients %in% valitud_aastad$med_ingredients),]%>%filter((age >= input$age_slider[1])&(age <= input$age_slider[2])& substr(ATC_CODE, 1, input$atc_level)%in%input$check2 & gender %in% input$gender)

      })

    # Sorting asc
    observeEvent(input$a2z, {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", choices = sort(unique(substr(total$ATC_CODE, 1, input$atc_level))), selected = unique(substr(total$ATC_CODE, 1, input$atc_level))
      )
    })
    # Sorting desc
    observeEvent(input$z2a, {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", choices = rev(sort(unique(substr(total$ATC_CODE, 1, input$atc_level)))), selected = unique(substr(total$ATC_CODE, 1, input$atc_level))
      )
    })
    observeEvent(input$all, {
      if (is.null(input$check2)) {
        updateCheckboxGroupInput(
          session = session, inputId = "check2", selected = unique(substr(total$ATC_CODE, 1, input$atc_level))
        )
      } else {
        updateCheckboxGroupInput(
          session = session, inputId = "check2", selected = ""
        )
      }
    })
    observeEvent(input$atc_level,{
      updateCheckboxGroupInput(
        session = session, inputId = "check2", choices = unique(substr(total$ATC_CODE, 1, input$atc_level)), selected = unique(substr(total$ATC_CODE, 1, input$atc_level))
      )
    })

    output$gender_plot<- renderPlotly({

      p1<-digit_counts()%>%group_by(PERSON_ID, gender)%>%summarise(CMA = mean(CMA))%>%ggplot(aes(x=gender, y=CMA, fill=gender))+
        geom_boxplot()+
        stat_summary(
          fun.data = n_fun,
          geom = "text",
          position = position_dodge2(preserve = "single",0.75, padding = 0.01),
          size = 3
        )+
        theme(legend.position = "none")
      ggplotly(p1)

    })
    output$plot2<-renderPlot({
      digit_counts()%>%group_by(ravim=substr(ATC_CODE, 1,input$atc_level))%>%summarise(kesk=mean(CMA))%>%
        ggplot(aes(x=reorder(ravim, -kesk), y=kesk))+
        geom_bar(stat="identity",fill="steelblue")+
        labs(x="medication",
             y="Average adherence",
             title="Average adherence of observed medication")+
        geom_text(aes(label=round(kesk, 2)), vjust=1.6, color="white", size=3.5)+
        theme_minimal()+
        theme(plot.title = element_text(size = 18, face = "bold"))
    })
    output$plot3<-renderPlotly({
      p3<-ggplot(digit_counts()%>%group_by(Ages=AgeGroup, PERSON_ID)%>%summarise(CMA=mean(CMA)), aes(x=Ages, y=CMA, fill=Ages))+
        geom_boxplot(alpha=0.3)+
        stat_summary(
          fun.data = n_fun,
          geom = "text",
          position = position_dodge2(preserve = "single",0.75, padding = 0.01),
          size = 3
        )+
        stat_summary(fun = median,
                     geom = "line",
                     col='blue')+
        theme(legend.position="none")
      ggplotly(p3)
    })
    output$adherence_change_gender<-renderPlot({
      ggplot(digit_counts()%>%group_by(gender, PERSON_ID, year = window.ID)%>%summarise(avg=mean(CMA))%>%group_by(gender,year)%>%summarise(CMA=mean(avg)), aes(x=year, y=CMA, color=gender))+
        geom_line()+
        geom_point()+
        geom_label_repel(aes(label = round(CMA,3)),
                         box.padding   = 0.35,
                         point.padding = 0.5,
                         segment.color = 'grey50')
    })

    output$adherence_change_ages<-renderPlot({
      ggplot(digit_counts()%>%group_by(AgeGroup, PERSON_ID, year = window.ID)%>%summarise(avg=mean(CMA))%>%group_by(Ages=AgeGroup,year)%>%summarise(CMA=mean(avg)), aes(x=year, y=CMA, color=Ages))+
        geom_line()+
        geom_point()+
        geom_label_repel(aes(label = round(CMA,3)),
                         box.padding   = 0.35,
                         point.padding = 0.5,
                         segment.color = 'grey50')
    })

  }
  return(shinyApp(ui = ui, server = server))
}


