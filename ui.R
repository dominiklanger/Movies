# Kudos to: http://deanattali.com/2015/06/14/mimicking-google-form-shiny/

library(shiny)
library(shinyjs)

labelMandatory <- function(label) {
      tagList(
            label,
            span("*", class = "mandatory_star")
      )
}

appCSS <-
   ".mandatory_star { color: red; }
   #error { color: red; }"

shinyUI(
      fluidPage(            
            shinyjs::useShinyjs(),
            shinyjs::inlineCSS(appCSS),
            
            # Application title
            titlePanel("Bernie's & Domi's Movie Database"),
            downloadButton("downloadBtn", label = "Download data"),
            DT::dataTableOutput("moviesTable"),
            div(
                  id = "form",
                  textInput("movieTitle", labelMandatory("Movie title")),
                  selectInput("movieYear", 
                              label = labelMandatory("Year"),
                              choices = 1990:2015,
                              selected = 2015
                  ),
                  selectInput("movieRating", 
                        label = labelMandatory("Rating"),
                        choices = 1:10,
                        selected = 5
                  ),
                  actionButton("submit", "Submit", class = "btn-primary"),
                  actionButton("delete", "Delete", class = "btn-primary")
            ),
            shinyjs::hidden(
                  span(id = "submit_msg", "Submitting..."),
                  div(id = "error",
                      div(br(), tags$b("Error: "), span(id = "error_msg"))
                  )
            ),
            shinyjs::hidden(
                  div(
                        id = "thankyou_msg",
                        h3("Thanks, your movie was submitted successfully!"),
                        actionLink("submit_another", "Submit another movie")
                  )
            )  
      )
)
