#' Interactive adherence dashboard with easy analysis genereated with summarized function adherence_summary.
#'
#' @param summarized_adherence Summarized table generated with function adherence_summary
#'
#' @return returns Shiny dashboard with interactive possibilities to observe adherence in different groupings. Using adherence summary and dashboard for said table is more efficient and optimized way to analyse populations adherence
#' @export
#'
#' @examples adherence_dashboard_summarized(summarized_table)
adherence_dashboard_summarized <- function(summarized_adherence) {
  total <- summarized_adherence

  ui <- dashboardPage(
    dashboardHeader(title = "Medication adherence"),
    dashboardSidebar(
      sidebarMenu(
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
          radioButtons(
            "atc_level2",
            label = "Filter by ATC level",
            choices = list(
              "I level" = 1,
              "II level" = 3,
              "III level" = 4,
              "IV level" = 5,
              "V level" = 7
            ),
            selected = 1
          ),
          br(),

          radioButtons(
            inputId = "check2",
            label = "Choose",
            choices = sort(unique(
              total %>% filter(nchar(ATC) == 1) %>% pull(ATC)
            )),
            selected = sort(unique(
              total %>% filter(nchar(ATC) == 1) %>% pull(ATC)
            ))[1]
          )
        ),
        radioButtons(
          "gender",
          label = "Observed gender",
          choices = c(unique(total$Gender)),
          selected = c(unique(total$Gender))[3]
        ),
        radioButtons(
          "age_group",
          label = "Observed Age group",
          choices = sort(unique(total$Age_group)),
          selected = sort(unique(total$Age_group))[1]
        ),
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
        ),
        radioButtons(
          "how_long",
          label = "How many years have people taken the medication (minimum)?",
          choices = c(sort(unique(
            total$years_taken
          ))),

          selected = sort(unique(total$years_taken))[1]
        )

      )

    ),
    dashboardBody(
      fluidRow(column(5, tableOutput("table"), offset = 3)),
      tabsetPanel(
        type = 'tabs',
        tabPanel("Gender",
                 fluidRow(column(
                   8, plotOutput("gender_box"), offset = 2
                 )),
                 fluidRow(column(
                   8, plotlyOutput("gender_change"), offset = 2
                 )),),
        tabPanel("Age",
                 fluidRow(column(
                   8, plotOutput("age_box"), offset = 2
                 )),
                 fluidRow(column(
                   8, plotlyOutput("age_change"), offset = 2
                 )),),
        tabPanel(
          "Medications",
          fluidRow(column(8, plotlyOutput("drug_avg"), offset = 2)),
          fluidRow(column(8, plotOutput("drug_diff"), offset = 2)),
          fluidRow(column(
            8, plotlyOutput("drug_change"), offset = 2
          )),
        )

      ),

    ),
    tags$head(
      tags$style(
        '
  #check2{
    font-family: "Arial";
    color: black}
  #atc_level2{
    font-family:"Arial";
    color:black};'
      )
    ),



  )

  server <- function(input, output, session) {
    data <- reactive({
      total %>% filter(
        ATC %in% input$check2 &
          Age_group %in% input$age_group &
          Gender %in% input$gender & years_taken == input$how_long
      )

    })
    ravimid <- reactive({
      if (input$check2 == 0) {
        total %>% filter(ATC != 0)
      }
      else{
        total %>% filter(substr(ATC, 1, input$atc_level2) == input$check2)
      }
    })

    #atc_level setting
    observeEvent(input$atc_level2, {
      updateRadioButtons(
        session = session,
        inputId = "check2",
        choices = sort(unique(
          total %>% filter(nchar(ATC) == input$atc_level2) %>% pull(ATC)
        )),
        selected = sort(unique(
          total %>% filter(nchar(ATC) == input$atc_level2) %>% pull(ATC)
        ))[1]
      )
    })
    # Sorting asc
    observeEvent(input$a2z, {
      updateRadioButtons(
        session = session,
        inputId = "check2",
        choices = sort(unique(
          total %>% filter(nchar(ATC) == input$atc_level2) %>% pull(ATC)
        )),
        selected = sort(unique(
          total %>% filter(nchar(ATC) == input$atc_level2) %>% pull(ATC)
        ))[1]
      )
    })
    # Sorting desc
    observeEvent(input$z2a, {
      updateRadioButtons(
        session = session,
        inputId = "check2",
        choices = rev(sort(unique(
          total %>% filter(nchar(ATC) == input$atc_level2) %>% pull(ATC)
        ))),
        selected = sort(unique(
          total %>% filter(nchar(ATC) == input$atc_level2) %>% pull(ATC)
        ))[1]
      )
    })

    output$table <- renderTable({
      data()
    })
    output$gender_box <- renderPlot({
      ggplot(
        total %>% filter(
          Gender != 0 &
            ATC %in% input$check2 &
            Age_group == input$age_group &
            Window_id == 0 & years_taken == input$how_long
        )
      ) +
        aes(
          x = Gender,
          ymin = min,
          ymax = max,
          lower = q25,
          middle = median,
          upper = q75,
          fill = Gender
        ) +
        geom_boxplot(stat = 'identity') +
        geom_text(aes(
          x = Gender,
          y = median,
          label = paste(
            'n =',
            count,
            '\n',
            'avg =',
            round(avg, 4),
            '\n',
            'med =',
            round(median, 4),
            '\n'
          )
        ), size = 5) +
        theme(
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          text = element_text(size = 12)
        )+
        labs(x='Gender', y='CMA')


    })

    output$age_box <- renderPlot({
      ggplot(
        total %>% filter(
          Gender == input$gender &
            ATC %in% input$check2 &
            Age_group != 0 & Window_id == 0 & years_taken == input$how_long
        )
      ) +
        aes(
          x = Age_group,
          ymin = min,
          ymax = max,
          lower = q25,
          middle = median,
          upper = q75,
          fill = Age_group
        ) +
        geom_boxplot(stat = 'identity') +
        geom_text(aes(
          x = Age_group,
          y = median,
          label = paste(
            'n =',
            count,
            '\n',
            'avg =',
            round(avg, 4),
            '\n',
            'med =',
            round(median, 4),
            '\n'
          )
        ),
        size = 4,
        vjust = .65) +
        theme(
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          text = element_text(size = 12)
        ) +
        coord_flip()+
        labs(x='Age group', y='CMA')

    })
    output$gender_change <- renderPlotly({
      gc <-
        ggplot(
          total %>% filter(
            Gender != 0 &
              ATC %in% input$check2 &
              Age_group == input$age_group &
              Window_id != 0 &
              years_taken == input$how_long
          ),
          aes(x = Window_id, y = avg, color = Gender)
        ) +
        geom_line() +
        geom_point()+
        labs(x='Year used', y='average CMA')
      ggplotly(gc)


    })
    output$age_change <- renderPlotly({
      ac <-
        ggplot(
          total %>% filter(
            Gender == input$gender &
              ATC %in% input$check2 &
              Age_group != 0 &
              Window_id != 0 &
              years_taken == input$how_long
          ),
          aes(x = Window_id, y = avg, color = Age_group)
        ) +
        geom_line() +
        geom_point()+
        labs(x='Year used', y='average CMA')

      ggplotly(ac)

    })

    output$drug_avg <- renderPlotly({
      da <-
        ggplot(
          ravimid() %>% filter(
            Gender == input$gender &
              Window_id == 0 &
              Age_group == input$age_group &
              str_length(ATC) == input$atc_level &
              years_taken == input$how_long
          ),
          aes(x = reorder(ATC, count), y = count)
        ) +
        geom_bar(stat = 'identity', aes(fill = avg), alpha = .9) +
        scale_fill_gradient(low = "#F8766D", high = "619CFF") +
        labs(x = "Medication groups",
             y = "How many different people purchased the medication") +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          text = element_text(size = 10)
        ) +
        coord_flip()
      ggplotly(da)
    })
    output$drug_diff <- renderPlot({
      ggplot(
        ravimid() %>% filter(

          str_length(ATC) == input$atc_level &
            Gender == input$gender &
            Age_group == input$age_group &
            Window_id == 0 & years_taken == input$how_long
        )
      ) +
        aes(
          x = ATC,
          ymin = min,
          ymax = max,
          lower = q25,
          upper = q75,
          middle = median,
          fill = ATC
        ) +
        geom_boxplot(stat = 'identity') +
        geom_text(aes(
          x = ATC,
          y = median,
          label = paste(
            'n =',
            count,
            '\n',
            'avg =',
            round(avg, 4),
            '\n',
            'med =',
            round(median, 4),
            '\n'
          )
        ), size = 3.5) +
        theme(
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          text = element_text(size = 12)
        ) +
        coord_flip()+
        labs(x='Medications', y='CMA')
    })

    output$drug_change <- renderPlotly({
      dc <-
        ggplot(
          ravimid() %>% filter(
            Gender == input$gender &
              Age_group == input$age_group &
              Window_id != 0 &
              years_taken == input$how_long & str_length(ATC) == input$atc_level
          )
        ) +
        aes(x = Window_id, y = avg, color = ATC) +
        geom_line() +
        geom_point()+
        labs(x='Year used', y='Average CMA')

      ggplotly(dc)
    })


  }


  return(shinyApp(ui, server))

}

