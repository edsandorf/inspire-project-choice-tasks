pkgs <- c("shiny", "shinyjs", "shinyWidgets", "DT", "dplyr")
invisible(lapply(pkgs, require, character.only = TRUE))

production <- TRUE

# UI ----
ui <- fluidPage(
  theme = "master.css",
  # Initialize shinyJS()
  shinyjs::useShinyjs(),
                
  # Create a custom command to unbind the radio buttons on re-draw of the table
  tags$script(
    HTML(
      "Shiny.addCustomMessageHandler('unbind-DT', function(id) {
        Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
      })"
    )
  ),
                
  # Title page
  fluidRow(
    class = "top-panel",
    column(8),
    column(
      4,
      img(src = "mono-reverse-logo.png", class = "funder-panel-image", style = "border:0;")
    )
  ),
  fluidRow(
    class = "treatment-panel",
    column(
      12,
      shiny::selectInput(
        inputId = "treatment",
        label = "Please select treatment: ",
        choices = seq_len(10L),
        selected = 1,
        width = "100%"
      )
    )
  ),
  fluidRow(
    class = "main-panel",
    column(
      12,
      tabsetPanel(
        tabPanel("Description", uiOutput("description")),
        tabPanel("Video", uiOutput("video")),
        tabPanel(
          "Choice task",
          uiOutput("choice_task"),
          p(),
          div(
            shinyWidgets::actionBttn(
              inputId = "next_alt",
              label = "Search for another bottle of wine",
              style = "material-flat",
              color = "success"
            ),
            shinyWidgets::actionBttn(
              inputId = "next_page",
              label = "Continue to next question",
              style = "material-flat",
              color = "success"
            )
          )
        )
      )
    )
  ),
  fluidRow(
    class = "funder-panel",
    column(
      12,
      uiOutput("resp_id")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # Read in the choice tasks ----
  path <- file.path("www", "choice_tasks.csv")
  choice_tasks <- readr::read_csv(path)
  
  # Define reactive values ----
  current <- reactiveValues(
    alt = 1,
    time = 0,
    treatment = 1,
    task = 1
  )
  
  checked <- reactiveValues()
  
  time_delay <- sample(seq(0, 5000, 250), 10, replace = TRUE)
  
  # Observe when treatment changes ----
  observeEvent(
    {
      input[["treatment"]]
    },
    {
      response_id <- "response_1"
      
      # Manually trigger unbind-DT when the treatment changes
      session$sendCustomMessage('unbind-DT', response_id)
      
      # Reset the reactive values
      current$alt <- 1
      current$treatment <- input[["treatment"]]
      
      # Reset the checked values
      if (current$treatment %in% c(5, 6, 7)) {
        lapply(seq_len(10), function(x) {
          checked[[paste0("alt_", x)]] <- ""
        })
      }
      
      # Set the current time to first value of the time_delay vector
      if (current$treatment %in% c(8, 9, 10)) {
        current$time <- time_delay[1]
      }
      
      # Make sure that the next alternative button is toggled on
      shinyjs::toggleState(id = "next_alt", condition = TRUE)
    }
  )
  
  # Observe when the next alternative is revealed ----
  observeEvent(
    {
      input[["next_alt"]]
    },
    {
      response_id <- "response_1"
      
      # Store the current consideration set
      if (current$treatment %in% c(5, 6, 7)) {
        checkbox_names <- paste("considered", seq_len(current$alt), "task", current$task, sep = "_")
        checked_values <- vapply(checkbox_names, function (x) {
          isTRUE(input[[x]])
        }, logical(1))
        for (i in seq_along(checked_values)) {
          checked[[paste0("alt_", i)]] <- ifelse(isTRUE(checked_values[i]), "checked", "")
        }
      }
      
      # Manually trigger unbind-DT when the next alternative button is clicked
      session$sendCustomMessage('unbind-DT', response_id)
      
      # Update the current alternative counter
      current$alt <- current$alt + 1

      # Delay the enabling of the button for the search cost treatments
      if (current$treatment %in% c(8, 9, 10) && current$alt <= 9) {
        shinyjs::toggleState(id = "next_alt", condition = FALSE)
        shinyjs::delay(
          current$time, 
          shinyjs::toggleState(id = "next_alt", condition = TRUE)
        )
        # Update the current time
        if (current$treatment == 10) {
          current$time <- as.integer(time_delay[current$alt])
        }
      }
      
      # Check that we don't reveal more than 10 alternatives
      if (current$alt > 9) {
        shinyjs::toggleState(id = "next_alt", condition = FALSE)
      }
    }
  )
  
  # Observe what happens when the current alternative and treatment changes (render choice task) ----
  observeEvent(
    {
      current$alt
      current$treatment
    },
    {
      local(
        {
          response_id <- "response_1"
          output[[response_id]] <- DT::renderDataTable(
            {
              nalts <- 10
              ## Check the treatments ----
              if (current$treatment == 1) {
                current$alt <- 4
                nalts <- 4
              } else if (current$treatment == 2) {
                current$alt <- 7
                nalts <- 7
              } else if (current$treatment == 3) {
                current$alt <- 10
              }
              
              the_rows <- ((1 + (current$task - 1) * nalts):(current$task * nalts))[seq_len(current$alt)]
              
              # Subset the choice_tasks to only the current choice task
              task_matrix <- choice_tasks %>%
                slice(the_rows)
              
              # Add checkboxes if the respondent is in the consideration-set or current best
              if (current$treatment %in% c(5, 6, 7)) {
                checkboxes <- matrix(0, nrow = current$alt, ncol = 1L)
                for (i in seq_len(current$alt)) {
                  checkboxes[i, ] <- sprintf(
                    '<input type = "checkbox" value = "%s" id = "%s" %s/>',
                    i,
                    paste("considered", i, "task", current$task, sep = "_"),
                     checked[[paste0("alt_", i)]]
                  )
                }
                names_tmp <- colnames(task_matrix)
                task_matrix <- cbind(task_matrix, checkboxes)
                if (current$treatment == 5) {
                  colnames(task_matrix) <- c(names_tmp, "<b>I am considering this alternative</b>")
                } else {
                  colnames(task_matrix) <- c(names_tmp, "<b>I am considering these alternatives</b>")
                }
              }
              
              # Add the choice response
              radio_choice <- matrix(0, nrow = current$alt, ncol = 1L)
              for (i in seq_len(current$alt)) {
                radio_choice[i, ] <- sprintf(
                  '<input type = "radio" name = "%s" value = "%s"/>',
                  response_id,
                  seq_len(current$alt)[i])
              }
              
              # Combine with radio buttons and set dimension names
              names_tmp <- colnames(task_matrix)
              task_matrix <- cbind(task_matrix, radio_choice)
              colnames(task_matrix) <- c(names_tmp, "<b>I choose</b>")
              if (current$alt == 1) {
                rownames(task_matrix) <- "<p style = \"color: white;\">B</p>" # Hack to keep a white letter at the top for spacing
              } else {
                rownames(task_matrix) <- c(paste0("<p style = \"color: white;\">B</p>"),
                                           paste0("Bottle ", seq_len(current$alt - 1)))
              }
              
              # Return the matrix
              t(task_matrix)
              
            },
            escape = FALSE, server = FALSE, selection = "none", class = c("nowrap"),
            callback = DT::JS(
              paste0(
                "var last_col = null;
                table.on('mouseenter', 'td', function() {
                  var td = $(this);
                  var col_index = table.cell(this).index().columnVisible;
                  if (col_index !== last_col) {
                    $(table.cells().nodes()).removeClass('highlight');
                    $(table.column(col_index).nodes()).addClass('highlight');
                  }
                });
            
                table.on('mouseleave', function() {
                  $(table.cells().nodes()).removeClass('highlight');
                });"
              )
            ),
            options = list(
              dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE,
              columnDefs = list(
                list(
                  className = "dt-center",
                  targets = seq_len(current$alt)
                )
              ),
              drawCallback = DT::JS(
                paste0("function() {
                          Shiny.unbindAll(this.api().table().node()); ",
                          paste0("var $radio_row = $(\"tr:has(input[name =", paste0("\'", response_id, "\'") ," ])\");"), 
                          "var $row = this.api().table().rows($radio_row);
                          var $this = $($row.nodes(0));",
                          paste0("$this.attr('id', ", paste0("\'", response_id, "\'"),");"),
                          paste0("$this.prop('checked', false);"),
                          "$this.addClass('shiny-input-radiogroup');
                          var objRow= $('table tbody tr:last');
                          $(objRow).addClass('highlight');",
                         "Shiny.bindAll(this.api().table().node());
                         $.fn.dataTableExt.errMode = 'none';
                        }"
                ) # End paste0()
              ) #  End DT::JS
            )
          ) # End renderDataTable()
        }
      )
    }
  )
  
  # Observe what happens when the current time changes ----
  observeEvent(
    {
      current$time
    },
    {
      output[["time_left"]] <- renderText(
        {
          left_on_timer <- as.character(current$time / 1000)
          if (nchar(left_on_timer) == 1) {
            left_on_timer <- paste0(left_on_timer, ".")
          }
          paste0(
            "The next alternative can be revealed in: ",
            stringr::str_pad(left_on_timer, 4, "right", "0"),
            "s"
          )
        }
      )
    }
  )
  
  # Description ----
  description_interface <- reactive(
    {
      return(
        shiny::withTags(
          div(
            h3("You have selected treatment ", current$treatment),
            if (current$treatment %in% c(1)) {
              p("Standard stated choice experiment with 3 alternatives and a 'buy none'.")
            },
            if (current$treatment %in% c(2)) {
              p("Standard stated choice experiment with 6 alternatives and a 'buy none'.")
            },
            if (current$treatment %in% c(3)) {
              p("Standard stated choice experiment with 9 alternatives and a 'buy none'.")
            },
            if (current$treatment %in% c(4)) {
              p("Sequential search stated choice experiment where a respondent could reveal up to 9 alternatives by clicking a button.")
            },
            if (current$treatment %in% c(5)) {
              p("Sequential search stated choice experiment where a respondent could reveal up to 9 alternatives by clicking a button. At each click of the button a respondent could indicate their current most preferred alternative. ")
            },
            if (current$treatment %in% c(6)) {
              p("Sequential search stated choice experiment where a respondent could reveal up to 9 alternatives by clicking a button. At each click of the button a respondent could indicate their current 3 most preferred alternatives.")
            },
            if (current$treatment %in% c(7)) {
              p("Sequential search stated choice experiment where a respondent could reveal up to 9 alternatives by clicking a button. At each click of the button a respondent could indicate all currently preferred alternatives.")
            },
            if (current$treatment %in% c(8)) {
              p("Sequential search stated choice experiment where a respondent could reveal up to 9 alternatives by clicking a button. Each click of the button has an associated search cost measured as the number of seconds to wait before the alternative is revealed. The search cost is constant across all alternatives and choice tasks. ")
            },
            if (current$treatment %in% c(9)) {
              p("Sequential search stated choice experiment where a respondent could reveal up to 9 alternatives by clicking a button. Each click of the button has an associated search cost measured as the number of seconds to wait before the alternative is revealed. The search cost is constant across all alternatives, but varies between choice tasks. ")
            },
            if (current$treatment %in% c(10)) {
              p(" Sequential search stated choice experiment where a respondent could reveal up to 9 alternatives by clicking a button. Each click of the button has an associated search cost measured as the number of seconds to wait before the alternative is revealed. The search cost varies across alternatives and choice tasks.")
            }
          )
        )
      )
    }
  )
  
  output[["description"]] <- renderUI(
    {
      description_interface()
    }
  )
  
  # Video ----
  video_interface <- reactive(
    {
      return(
        shiny::withTags(
          div(
            h3("You have selected to view the video for treatment ", current$treatment),
            p(
              "To show respondents how to answer the choice tasks, we created short instructional videos. We wanted the videos to present the information in a neutral way that was consistent across all treatments. 
            We created the videos using screen capture software and used Amazon Polly from AWS to create the voice-over. Respondents were instructed to watch the video carefully before proceeding with the choice tasks."
            ),
            p(
              "The video may take a few seconds to load, and in some browsers you may have to click the actual video to begin loading."
            ),
            if (current$treatment %in% c(1)) {
              video(id = "sample-video", type = "video/mp4", src = "treatment-01.mp4", controls = "controls",
                    width = 900, height = 450)
            },
            if (current$treatment %in% c(2)) {
              video(id = "sample-video", type = "video/mp4", src = "treatment-02.mp4", controls = "controls",
                    width = 900, height = 450)
            },
            if (current$treatment %in% c(3)) {
              video(id = "sample-video", type = "video/mp4", src = "treatment-03.mp4", controls = "controls",
                    width = 900, height = 450)
            },
            if (current$treatment %in% c(4)) {
              video(id = "sample-video", type = "video/mp4", src = "treatment-04.mp4", controls = "controls",
                    width = 900, height = 450)
            },
            if (current$treatment %in% c(5)) {
              video(id = "sample-video", type = "video/mp4", src = "treatment-05.mp4", controls = "controls",
                    width = 900, height = 450)
            },
            if (current$treatment %in% c(6)) {
              video(id = "sample-video", type = "video/mp4", src = "treatment-06.mp4", controls = "controls",
                    width = 900, height = 450)
            },
            if (current$treatment %in% c(7)) {
              video(id = "sample-video", type = "video/mp4", src = "treatment-07.mp4", controls = "controls",
                    width = 900, height = 450)
            },
            if (current$treatment %in% c(8, 9, 10)) {
              video(id = "sample-video", type = "video/mp4", src = "treatment-08-10.mp4", controls = "controls",
                    width = 900, height = 450)
            }
          )
        )
      )
    }
  )
  
  output[["video"]] <- renderUI(
    {
      video_interface()
    }
  )
  
  # Choice task ----
  choice_task_interface <- reactive(
    {
      response_id <- "response_1"
      
      # Toggle the visibility of the next alternative button
      shinyjs::hideElement(id = "next_alt")
      if (current$treatment %in% c(4:10)) {
        shinyjs::showElement(id = "next_alt")
      }
      
      # The output observer ----
      observe(
        {
          # Set the observers for shinyJS
          if (current$treatment %in% c(5, 6, 7)) {
            checkbox_names <- paste("considered", seq_len(current$alt), "task", 1, sep = "_")
            checked <- vapply(checkbox_names, function(x) {
              isTRUE(input[[x]])
            }, logical(1))
            sum_checked <- sum(checked)
            
            if (current$treatment == 5) toggle_input_condition <- 1
            if (current$treatment == 6) toggle_input_condition <- 3
            if (current$treatment == 7) toggle_input_condition <- 10
            
            if (sum_checked >= toggle_input_condition) {
              for (i in seq_len(current$alt)) {
                if (isFALSE(checked[i])) {
                  shinyjs::disable(id = checkbox_names[i])
                }
              }
            } else {
              for (i in seq_len(current$alt)) {
                shinyjs::enable(id = checkbox_names[i])
              }
            }
            
            consideration_check <- as.integer(input[[response_id]]) %in% which(checked %in% TRUE)
            
            output[["check_consideration_set"]] <- renderPrint(
              {
                str(sapply(checkbox_names, function(x) input[[x]]))
              }
            )
          } else {
            consideration_check <- TRUE
          } # End of if-statement
          
          toggle_condition <- length(input[[response_id]]) > 0 && consideration_check
          
          output[["check_answer"]] <- renderPrint(
            {
              str(input[[response_id]])
            }
          )
          
          # Toggle the next question button
          shinyjs::toggleState("next_page", condition = toggle_condition)
        }
      ) # End of observer
      
      # return() ----
      return(
        shiny::withTags(
          div(
            h3("You have selected treatment ", current$treatment),
            div(DT::dataTableOutput(response_id)),
            if (current$treatment %in% c(8, 9, 10)) {
              textOutput("time_left")
            },
            if (!production) {
              verbatimTextOutput("check_answer")
            },
            if (!production) {
              verbatimTextOutput("check_consideration_set")
            }
          )
        )
      )
    }
  )
  
  output[["choice_task"]] <- renderUI(
    {
      choice_task_interface()
    }
  )
}

# Combine into application ----
shinyApp(ui = ui, server = server)
