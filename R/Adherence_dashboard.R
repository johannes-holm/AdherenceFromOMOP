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


  ui <- fluidPage(

    titlePanel("Adherence"),

    fixedRow(

      column(
        3,

        sliderInput(
          inputId = "slider",
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
        radioButtons(
          "gender",
          label = "Observed gender",
          choices = list(
            "Male" = "M",
            "Female" = "F",
            "All" = "A"
          ),
          selected = "A"
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
          choices = list(
            "1 year" = 1,
            "2 years" = 3,
            "3 years" = 6,
            "4 years" = 10,
            "5 years" = 15,
            "6 years" = 21,
            "7 years" = 28,
            "8 years" = 36
          ),
          selected = 1
        )
      ),
    ),

    fluidRow(
      column(5,plotlyOutput("plot"), offset=3)),
    fluidRow(
      column(5,plotOutput("plot2"), offset=3)),
    fluidRow(
      column(10,plotlyOutput("plot3"), offset=1))
  )

  server <- function(input, output, session) {
    digit_counts <- reactive({
      if(input$gender == "A"){
        p<-total%>%filter((age >= input$slider[1])&(age <= input$slider[2])& substr(ATC_CODE, 1, input$atc_level)%in%input$check2)
        test<-p%>%group_by(PERSON_ID, ATC_CODE)%>%summarise(summa = sum(window.ID))%>%filter(summa>=input$how_long)
        p<-merge(p, test, all=FALSE, by=c("PERSON_ID","ATC_CODE"))
      }
      else{
        p<-total%>%filter((age >= input$slider[1])&(age <= input$slider[2])& substr(ATC_CODE, 1, input$atc_level) %in%input$check2 & gender == input$gender)
        test<-p%>%group_by(PERSON_ID, ATC_CODE)%>%summarise(summa = sum(window.ID))%>%filter(summa>=input$how_long)
        p<-merge(p, test, all=FALSE, by=c("PERSON_ID","ATC_CODE"))
      }})

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

    output$plot<- renderPlotly({
      p1<-digit_counts()%>%group_by(year=window.ID, gender=gender)%>%summarise(avg = mean(CMA))%>%
        ggplot(aes(x=year, y =avg, group = gender, color = gender))+
        geom_line()+
        labs(title = "Average adherence through 7 years")+
        scale_x_discrete(limits=c(1,2,3,4,5,6,7,8))+
        theme(plot.title = element_text(size = 13, face = "bold"))
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
      p3<-ggplot(digit_counts()%>%group_by(vanused = AgeGroup, Year = window.ID, Gender = gender)%>%summarise(cma=mean(CMA)), aes(x=Year, y=cma, group=Gender, color=Gender))+
        geom_line()+
        labs(title = "CMA 3 observed by age groups through 7 years")+
        scale_x_discrete(limits=c(1,2,3,4,5,6,7,8))+
        facet_grid(~vanused)+
        theme(plot.title = element_text(size = 13, face = "bold"))
      ggplotly(p3)
    })

  }
  return(shinyApp(ui = ui, server = server))
}
