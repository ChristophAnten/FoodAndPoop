
ui <- dashboardPage(
  
  # ########################################################################## dashboardHeader ####
  dashboardHeader(title = "Poop"),
  
  # ######################################################################### dashboardSidebar ####
  dashboardSidebar(collapsed = TRUE,
                   actionButton("showShitlist",
                                "showShitlist"),
                   actionButton("showTimeInput",
                                "showTimeInput"),
                   actionButton("showDateInput",
                                "showDateInput"),
                   actionButton("showTemplates",
                                "showTemplates"),
                   actionButton("exportXLS",
                                "exportXLS"),
                   actionButton("saveTest",
                                "saveTest")),
  
  # ############################################################################ dashboardBody ####
  dashboardBody(
    shinyjs::useShinyjs(),
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(
        width = 4,
        #======================================================================== Add shit Box ====
        box(title = "How was your shit today?",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            fluidRow(
              column(5,
                     dateInput("shit_date",
                               label = "When did you shit?")
              ),
              column(4,
                     timeInput("shit_time","And what time?", 
                               value = Sys.time(), 
                               seconds = FALSE)
              ),
              column(3,
                     strong("Reset date/time"),
                     actionButton("shit_heute",
                                  "Now!",
                                  width = "100%"))
            ),
            hr(), # ------------------------------------------------------------------------------ -
            
            selectInput("shit_templates",
                        label = "Choose a template (optional)!",
                        choices = sort(shit_template_names)),
            sliderTextInput("shit_thickness", 
                            label = h5(tags$b("How thick was your shit?")), 
                            choices = thickness, selected = thickness[6]
            ),
            sliderTextInput("shit_debree", 
                            label = h5(tags$b("Where there any undygested pieces?")), 
                            choices = pieces, selected = pieces[6]
            ),
            fluidRow(
              column(3,
                     strong("Fart a lot?")
              ),
              column(9,
                     radioButtons("radio_fart",
                                  label = NULL,
                                  c("Don't know!" = NA,
                                    "None" = 0,
                                    "a little" = 1,
                                    "medium" = 2,
                                    "a lot" = 3),
                                  inline = TRUE
                     )
              )
            ),
            fluidRow(
              column(3,
                     strong("Bloated stomach?")
              ),
              column(9,
                     radioButtons("radio_bloat",
                                  label = NULL,
                                  c("Don't know!" = NA,
                                    "None" = 0,
                                    "a little" = 1,
                                    "medium" = 2,
                                    "a lot" = 3),
                                  inline = TRUE
                     )
              )
            ),
            fluidRow(
              column(3,
                     strong("Heartburn?")
              ),
              column(9,
                     radioButtons("radio_heartburn",
                                  label = NULL,
                                  c("Don't know!" = NA,
                                    "None" = 0,
                                    "a little" = 1,
                                    "medium" = 2,
                                    "a lot" = 3),
                                  inline = TRUE
                     )
              )
            ),
            fluidRow(
              column(3,
                     strong("Pain?")
              ),
              column(9,
                     radioButtons("radio_pain",
                                  label = NULL,
                                  c("Don't know!" = NA,
                                    "None" = 0,
                                    "a little" = 1,
                                    "medium" = 2,
                                    "a lot" = 3),
                                  inline = TRUE
                     )
              )
            ),
            
            colourpicker::colourInput("shit_colour",
                        label = h5(tags$b("What was the colour?")),
                        value = "#543817"),
            textAreaInput("shit_comment",
                          label = "Add a shit comment?",
                          placeholder = "Will not be added to template!",
                          rows = 3),
            
            hr(), # ------------------------------------------------------------------------------ -
            fluidRow(
              column(6,
                     textInput("shit_templateName",
                               label = NULL,
                               placeholder = "template name (optional)")
              ),
              column(6,
                     div(actionButton("shit_save","Save",icon = icon("poo"),
                                      style=styles$submit), style = "float: right")
              )
            )
        ),
        #======================================================================== shit Box END ====
        
        #======================================================================== Add food Box ====
        box(title = "What did you consume?",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              column(6,
                     dateInput("food_date",
                               label = "When did you shit?")
              ),
              column(6,
                     timeInput("food_time","And what time?", 
                               value = Sys.time(), 
                               seconds = FALSE)
              )
            ),
            hr(), # ------------------------------------------------------------------------------ -
            selectInput("food_templates",
                        label = "Choose a template (optional)!",
                        choices = sort(food_default),
                        multiple=TRUE),
            
            actionButton("food_add",
                         "Add new recipe/food"),
            
            
            textAreaInput("food_comment",
                          label = "Add a Food comment?",
                          placeholder = "Will not be added to template!",
                          rows = 3),
            hr(), # ------------------------------------------------------------------------------ -
            fluidRow(
              column(6,
                     textInput("food_templateName",
                               label = NULL,
                               placeholder = "template name (optional)")
              ),
              column(6,
                     div(actionButton("food_save","Save",style=styles$submit), style = "float: right")
              )
            )
        )
        #======================================================================== food Box END ====
      ),  
      
      column(
        width = 8,
        #================================================================== add shit table Box ====
        
        box(
          width = 12,
          title = "Shittable",
          collapsible = TRUE,
          DT::dataTableOutput("shitlist_items",width = "100%"),
          hr(),
          actionButton("shitlist_remove","Remove selection",icon = icon("trash-alt")),
          actionButton("shitlist_edit","Edit entry",icon = icon("edit")),
          div(actionButton("shitlist_export","Export .xlsx",icon = icon("file-export"),
                           style=styles$submit), style = "float: right")
        ),
        
        #================================================================== shit table Box END ====
        
        #================================================================== add food table Box ====
        
        box(
          width = 12,
          title = "Foodtable",
          collapsible = TRUE,
          DT::dataTableOutput("foodlist_items",width = "100%")
        )
        
        #================================================================== food table Box END ====
      )
    )
  )
  # ###################################################################################### END ####
  
)

