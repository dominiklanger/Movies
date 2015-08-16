# Kudos to: http://deanattali.com/2015/06/14/mimicking-google-form-shiny/

library(shiny)
library(dplyr)
library(DT)

fieldsMandatory <- c("movieTitle", "movieYear", "movieRating")
fieldsAll <- c("movieTitle", "movieYear", "movieRating")
responsesDir <- file.path("data")

epochTime <- function() {
      as.integer(Sys.time())
}

humanTime <- function() {
      format(Sys.time(), "%Y%m%d-%H%M%OS")
}

loadData <- function() {
      files <- list.files(file.path(responsesDir), full.names = TRUE)
      data <- lapply(files, read.csv, stringsAsFactors = FALSE)
      data <- dplyr::rbind_all(data)
      data
}

saveData <- function(data) {
      fileName <- sprintf("%s_%s.csv",
                          humanTime(),
                          digest::digest(data))
      
      write.csv(x = data, file = file.path(responsesDir, fileName),
                row.names = FALSE, quote = TRUE)
}

removeData <- function(index) {
      files <- list.files(file.path(responsesDir), full.names = TRUE)
      file.remove(files[index])
}

shinyServer(function(input, output, session) {
      values <- reactiveValues()      
      values$reloadTable <- FALSE
      shinyjs::disable("delete")
      
      tableData <- reactive({
            values$reloadTable # needed to trigger reloading the table if the value of values$reloadTable has changed
            
            data <- loadData() 
            if (nrow(data) > 0)      
                  select(data, -timestamp)
            else 
                  data
      })
      
      formData <- reactive({
            data <- sapply(fieldsAll, function(x) input[[x]])
            data <- c(data, timestamp = epochTime())
            data <- t(data)
            data
      })
      
      observe({
            # check if all mandatory fields have a value
            mandatoryFilled <-
                  vapply(fieldsMandatory,
                         function(x) {
                               !is.null(input[[x]]) && input[[x]] != ""
                         },
                         logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            # enable/disable the submit button
            shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
      })
      
      # action to take when submit button is pressed
      observeEvent(input$submit, {
            shinyjs::disable("submit")
            shinyjs::show("submit_msg")
            shinyjs::hide("error")
            
            indices <- input$moviesTable_rows_selected
            if (length(indices))
                  removeData(indices)
            
            tryCatch({
                  saveData(formData())
                  values$reloadTable <- !values$reloadTable
                  shinyjs::reset("form")
                  shinyjs::hide("form")
                  shinyjs::show("thankyou_msg")
            },
            error = function(err) {
                  shinyjs::text("error_msg", err$message)
                  shinyjs::show(id = "error", anim = TRUE, animType = "fade")
            },
            finally = {
                  shinyjs::enable("submit")
                  shinyjs::hide("submit_msg")
            })
      })
      
      # action to take when delete button is pressed
      observeEvent(input$delete, {
            indices <- input$moviesTable_rows_selected
            if (length(indices)) {
                  removeData(indices)
            
                  values$reloadTable <- !values$reloadTable
                  shinyjs::reset("form")
                  shinyjs::disable("delete")                  
            }
      })
      
      observeEvent(input$moviesTable_rows_selected, {
            indices <- input$moviesTable_rows_selected
            if (length(indices)) {
                  movie <- loadData()[input$moviesTable_rows_selected, ]
                  updateTextInput(session, "movieTitle", value = movie$movieTitle)
                  updateSelectInput(session, "movieYear", selected = movie$movieYear)
                  updateSelectInput(session, "movieRating", selected = movie$movieRating)
                  shinyjs::enable("delete")
            }
            else {
                  shinyjs::disable("delete")
            }
      })
      
            
      observeEvent(input$submit_another, {
            shinyjs::show("form")
            shinyjs::hide("thankyou_msg")
      })  
      
      output$moviesTable <- DT::renderDataTable(            
            tableData(),
            server = FALSE, 
            rownames = FALSE,
            colnames = c('Title', 'Year', 'Rating'),
            selection = 'single',
            options = list(searching = FALSE, lengthChange = FALSE, paging = TRUE, escape = FALSE)
      ) 
      
      output$downloadBtn <- downloadHandler(
            filename = function() { 
                  sprintf("movies_%s.csv", humanTime())
            },
            content = function(file) {
                  write.csv(tableData(), file, row.names = FALSE)
            },
            contentType = "text/csv"
      )
      

})
