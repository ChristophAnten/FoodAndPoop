
server <- function(input, output,session) {
  # ################################################################################## functions ####
  add_defecation <- function(name=NULL,
                             time=NULL,
                             date=NULL,
                             density=NULL,
                             pieces=NULL,
                             colour=NULL,
                             fart=NULL,
                             bloat=NULL,
                             heartburn=NULL,
                             pain=NULL,
                             comment=NULL,
                             template=FALSE){
    if (template){
      if (name %in% names(react$templates)){
        updateTextInput(session,
                        inputId = "shit_templateName", 
                        value = "",
                        placeholder = "This name already exists!")
      } else {
        react$templates[[name]] <- list(date=date,
                                        time=strftime(time, format="%H:%M:%S"),
                                        density=density,
                                        pieces=pieces,
                                        colour=colour,
                                        fart=fart,
                                        bloat=bloat,
                                        heartburn=heartburn,
                                        pain=pain,
                                        comment=comment)
      }
    } else {
      react$shitlist[[newId("shit")]] <- list(name=name,
                                              date=date,
                                              time=strftime(time, format="%H:%M:%S"),
                                              density=density,
                                              pieces=pieces,
                                              colour=colour,
                                              fart=fart,
                                              bloat=bloat,
                                              heartburn=heartburn,
                                              pain=pain,
                                              comment=comment)
    }
  }
  
  newId <- function(type=NULL){
    if(is.null(type))
      type="id"
    if(length(names(react$shitlist))==0)
      return(sprintf("%s_0",type))
    return(sprintf("%s_%i",type,length(names(react$shitlist))))
  }
  
  get_thicknessVal <- function(x){
    which(x==thickness)-1
  }
  get_piecesVal <- function(x){
    which(x==pieces)-1
  }
  
  # ################################################################################## variables ####
  
  react <- reactiveValues(shitlist = list(),
                          templates = list(),
                          foods = list())
  
  if (exists("shitlist"))
    react$shitlist = shitlist
  
  if (exists("templates"))
    react$templates = templates
  
  if (exists("foods"))
    react$foods = foods
  
  # ################################################################################## shit Box ####
  observeEvent(input$shit_heute,{
    updateTimeInput(session,
                    "shit_time",
                    value=Sys.time())
    updateDateInput(session,
                    "shit_date",
                    value=Sys.Date())
  })
  
  observeEvent(input$shit_templates,{
    if (input$shit_templates != shit_template_names_default){
      updateSliderTextInput(session,
                        inputId = "shit_thickness",
                        selected = react$templates[[input$shit_templates]]$density)
      updateSliderTextInput(session,
                        inputId = "shit_debree",
                        selected = react$templates[[input$shit_templates]]$pieces)
      colourpicker::updateColourInput(session,
                        inputId = "shit_colour",
                        value = react$templates[[input$shit_templates]]$colour)
      updateRadioButtons(session,
                         inputId = "radio_fart",
                         select = as.numeric( react$templates[[input$shit_templates]]$fart))
      updateRadioButtons(session,
                         inputId = "radio_heartburn",
                         select = as.numeric( react$templates[[input$shit_templates]]$heartburn))
      updateRadioButtons(session,
                         inputId = "radio_bloat",
                         select = as.numeric( react$templates[[input$shit_templates]]$bloat))
      updateRadioButtons(session,
                         inputId = "radio_pain",
                         select = as.numeric( react$templates[[input$shit_templates]]$pain))
    }
  })
  
  observeEvent(input$shit_save,{           
    if(input$shit_templateName != ""){
      print("added_template")
      add_defecation(name=input$shit_templateName,
                     time=input$shit_time,
                     date=input$shit_date,
                     density=input$shit_thickness,
                     pieces=input$shit_debree,
                     colour=input$shit_colour,
                     fart=input$radio_fart,
                     bloat=input$radio_bloat,
                     heartburn=input$radio_heartburn,
                     pain=input$pain,
                     comment=input$shit_comment,
                     template = TRUE)
      updateSelectInput(session,
                        inputId = "shit_templates",
                        choices = c(shit_template_names, names(react$templates)))
      updateTextInput(session,
                      inputId = "shit_templateName",
                      value = "")
    }
    print("save_shit")
    add_defecation(name=input$shit_templateName,
                   time=input$shit_time,
                   date=input$shit_date,
                   density=get_thicknessVal(input$shit_thickness),
                   pieces=get_piecesVal(input$shit_debree),
                   colour=input$shit_colour,
                   fart=input$radio_fart,
                   bloat=input$radio_bloat,
                   heartburn=input$radio_heartburn,
                   pain=input$pain,
                   comment=input$shit_comment)
    updateTextAreaInput(session,inputId = "shit_comment",value = "")
  })
  
  # ################################################################################## food Box ####
  # --------------------------------------------------------- observeEvent input$food_templates ----
  old_food_selected = c()
  old_food_choices = sort(food_default)  
  idx <- length(old_food_choices)

  observeEvent(input$food_templates, {
    print
    req(!identical(old_food_selected, input$food_templates))
    
    addition <- base::setdiff(input$food_templates, old_food_selected)
    if (length(addition) > 0) {
      cat("step 2> addition:", addition, "\n")
      idx <<- idx + 1
      new_nm <- names(old_food_choices[old_food_choices == addition])
      new_val <- paste0(new_nm, idx)
      choices <- c(old_food_choices, new_val)
      names(choices) <- c(names(old_food_choices), new_nm)
    }
    
    missing <- base::setdiff(old_food_selected, input$food_templates)
    if (length(missing) > 0) {
      cat("step 2> missing:", missing, "\n")
      missing_idx <- which(old_food_choices == missing)
      choices <- old_food_choices[-missing_idx]
    }
    
    # sort choices so that No is always first
    choices <- sort(choices)
    cat("step 3> updated choices:", choices, "\n")
    
    updateSelectInput(session, "food_templates",
                      choices = choices,
                      selected = input$food_templates
    )
    
    # save current values
    old_food_selected <<- input$food_templates
    old_food_choices <<- choices
  }, ignoreNULL = FALSE)
  # ---------------------------------------------------------------- observeEvent input$food_add ----
  observeEvent(input$food_add,{
    showModal(
      modalDialog(
        title = "Add some food!",
        textInput("food_name",
                  label = NULL,
                  placeholder = "What is it called"),
        "Ingredients etc. (yet to come!)",
        hr(),
        fluidRow(
          column(12,
                 div(actionButton("save_food","add"), style = "float: right")       
          )
        ),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  # --------------------------------------------------------------- observeEvent input$save_food ----
  observeEvent(input$save_food,{
    food_default <<- LETTERS[1:4]
    old_food_selected <<- c()
    old_food_choices <<- sort(food_default)  
    idx <-- length(old_food_choices)
    
    # look at templates shit save
    # add_food()
    updateSelectInput(session,"food_templates",choices = food_default)
    removeModal()
  })
  # ################################################################################## table Box ####
  
  output$shitlist_items <- DT::renderDataTable({
    req(length(react$shitlist)>0)
    out <- map_dfr(react$shitlist, ~as_data_frame(t(.)))
    print(names(out))
    out <- DT::datatable(out,
                         selection = "multiple",
                         options = list(
                           scrollX = TRUE,
                           columnDefs = list(list(className = 'dt-right', targets = "_all"))
                         )) %>% 
      formatStyle(c("pieces","density"),
                  background = styleColorBar(0:10, 'lightblue'),
                  backgroundSize = '98% 40%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle("colour",
                  color = styleEqual(out$colour,out$colour),
                  fontWeight = "bold")
      
    out
  })
  
  observeEvent(input$shitlist_items_cell_clicked,{
    print(input$shitlist_items_rows_selected)
    if(is.null(input$shitlist_items_rows_selected)){
      shinyjs::disable("shitlist_edit")
      shinyjs::disable("shitlist_remove")
    }
    if(length(input$shitlist_items_rows_selected)==1){
      shinyjs::enable("shitlist_edit")
      shinyjs::enable("shitlist_remove")
    }
    if(length(input$shitlist_items_rows_selected)>1){
      shinyjs::disable("shitlist_edit")
      shinyjs::enable("shitlist_remove")
    }
  })
  
  observeEvent(input$shitlist_remove, {
    print(input$shitlist_items_rows_selected)
    showModal(
      modalDialog(
        h4("Are you sure to delete the selected poop?!"),
        footer = tagList(actionButton("shitlist_confirm_remove", "Yeah!",
                                      style=styles$submit),
                         modalButton("Cancel")
        )
      )
    )    
  })
  observeEvent(input$shitlist_confirm_remove,{
    react$shitlist[input$shitlist_items_rows_selected] <- NULL
    removeModal()
  })
  observeEvent(input$shitlist_edit, {
    #open shitlist item
    # and edit!!
  })
  
  # ################################################################################## control Box ####
  observeEvent(input$showShitlist,{
    print("length(names(react$shitlist))")
    print(length(names(react$shitlist)))
    print("react$shitlist")
    print(react$shitlist)
    print("react")
    print(react)
    print("as_data_frame(t(react$shitlist))")
    print(as_data_frame(t(react$shitlist)))
    
    print("map_dfr(react$shitlist, ~as_data_frame(t(.)))")
    print(map_dfr(react$shitlist, ~as_data_frame(t(.))))
  }
  )
  
  observeEvent(input$showTimeInput,{
    print("input$shit_time")
    print(input$shit_time)
  }
  )
  
  observeEvent(input$showDateInput,{
    print("input$shit_date")
    print(input$shit_date)
  }
  )
  
  observeEvent(input$showTemplates,{
    print("react$templates")
    print(react$templates)
  }
  )
  
  observeEvent(input$saveTest,{
    print("save(shitlist)")
    print(react$shitlist)
    isolate({
      react_save <- reactiveValuesToList(react)
      shitlist <- react_save$shitlist
      save(shitlist,file = "./www/shitlist.RData")
      
      templates <- react_save$templates
      save(templates,file = "./www/templates.RData")
      
      foods <- react_save$shitlist
      save(foods,file = "./www/foods.RData")
    })
  })
  
  observeEvent(input$saveTest,{
    print("save(shitlist)")
    print(react$shitlist)
    isolate({
      react_save <- reactiveValuesToList(react)
      shitlist <- react_save$shitlist
      save(shitlist,file = "./www/shitlist.RData")
      
      templates <- react_save$templates
      save(templates,file = "./www/templates.RData")
      
      foods <- react_save$shitlist
      save(foods,file = "./www/foods.RData")
    })
  })
  
}
