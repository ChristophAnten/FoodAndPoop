
server <- function(input, output,session) {
  # ################################################################################## functions ####
  # logged_in <- reactiveVal(FALSE)
  # 
  # # switch value of logged_in variable to TRUE after login succeeded
  # observeEvent(input$login, {
  #   logged_in(ifelse(logged_in(), FALSE, TRUE))
  # })
  # 
  # # show "Login" or "Logout" depending on whether logged out or in
  # output$logintext <- renderText({
  #   if(logged_in()) return("Logout here.")
  #   return("Login here")
  # })
  # 
  # # show text of logged in user
  # output$logged_user <- renderText({
  #   if(logged_in()) return("User 1 is logged in.")
  #   return("")
  # })
  observeEvent(input$quitApp,{
    showModal(
      modalDialog(
        h4("Quit without saving?"),
        modalButton("Cancel"),
        actionButton("quit_withoutSave","Quit"),
        div(actionButton("quit_withSave","Save & Quit",style = styles$submi),
            style = "float: right"),
        footer = NULL
      )
    )
  })
  observeEvent(input$quit_withSave,{
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
    stopApp()
  })
  
  observeEvent(input$quit_withoutSave,{
    stopApp()
  })
  
  add_defecation <- function(name=NULL, time=NULL, date=NULL,
                             density=NULL, pieces=NULL, colour=NULL,
                             bloat=NULL, fart=NULL, heartburn=NULL, pain=NULL,
                             comment=NULL){
    if(util$edit){
      react$shitlist[[input$shitlist_items_rows_selected]] <- list(
        name=input$shit_templateName,
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
      util$edit <- FALSE
    } else {
      react$shitlist[[newId("shit")]] <- list(
        name=input$shit_templateName,
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
    }
  }
  
  add_template <- function(){
    react$templates[[input$shit_templateName]] <- list(
      time=input$shit_time,
      date=input$shit_date,
      density=input$shit_thickness,
      pieces=input$shit_debree,
      colour=input$shit_colour,
      fart=input$radio_fart,
      bloat=input$radio_bloat,
      heartburn=input$radio_heartburn,
      pain=input$pain,
      comment=input$shit_comment)
  }

  get_defecation <- function(ListEntry){
    # not yet ...
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
  updateShitInput <- function(ShitListEntry){
    if(show.functionCall){print("#=== updateShitInput === START")}
    updateSliderTextInput(session,
                          inputId = "shit_thickness",
                          selected = ShitListEntry$density)
    updateSliderTextInput(session,
                          inputId = "shit_debree",
                          selected = ShitListEntry$pieces)
    colourpicker::updateColourInput(session,
                                    inputId = "shit_colour",
                                    value = ShitListEntry$colour)
    updateRadioButtons(session,
                       inputId = "radio_fart",
                       select = as.numeric( ShitListEntry$fart))
    updateRadioButtons(session,
                       inputId = "radio_heartburn",
                       select = as.numeric( ShitListEntry$heartburn))
    updateRadioButtons(session,
                       inputId = "radio_bloat",
                       select = as.numeric( ShitListEntry$bloat))
    updateRadioButtons(session,
                       inputId = "radio_pain",
                       select = as.numeric( ShitListEntry$pain))
    updateTextAreaInput(session,
                        inputId = "shit_comment",
                        value = ShitListEntry$comment)
    if(show.functionCall){print("#=== updateShitInput === END")}
  }
  
  # observeEvent(input$shit_comment,{
  #   print(paste("shot comment:",input$shit_comment))
  # })
  
  # ################################################################################## variables ####
  
  react <- reactiveValues(shitlist = list(),
                          templates = list(),
                          foods = list())
  util <- reactiveValues(edit = FALSE)
  
  if (exists("shitlist"))
    react$shitlist = shitlist
  
  if (exists("templates"))
    react$templates = templates
  
  if (exists("foods"))
    react$foods = foods
  
  # ################################################################################## shit Box ####
  # ---------------------------------------- observeEvent input$shit_heute ----
  observeEvent(input$shit_heute,{
    if(show.observeEvents)print("#---- observeEvent: input$shit_heute --- START")
    updateTimeInput(session,
                    "shit_time",
                    value=Sys.time())
    updateDateInput(session,
                    "shit_date",
                    value=Sys.Date())
    if(show.observeEvents)print("#---- observeEvent: input$shit_heute --- END")
  })
  # ---------------------------------------- observeEvent input$shit_templates ----
  observeEvent(input$shit_templates,{
    if(show.observeEvents)print("#---- observeEvent: input$shit_templates --- START")
    if (input$shit_templates != shit_template_names_default){
      updateShitInput(react$templates[[input$shit_templates]])
    }
    if(show.observeEvents)print("#---- observeEvent: input$shit_templates --- END")
  })
  
  # ---------------------------------------- observeEvent input$shit_save ----
  observeEvent(input$shit_save,{
    if(show.observeEvents)print("#---- observeEvent: input$shit_save --- START")
    print("util$edit")
    print(util$edit)
      updateActionButton(session,
                         inputId = "shit_save",
                         label = "Save")
    if(input$shit_templateName != ""){
      if (input$shit_templateName %in% names(react$templates)){
        showModal(
          modalDialog(
            h4("Do you want to override the existing template with the same name?"),
            footer = tagList(actionButton("template_confirm_override", "Yeah!",
                                          style=styles$submit),
                             modalButton("Cancel")
            )
          )
        )
      } else {
        add_template()
        add_defecation()
        updateSelectInput(session,
                          inputId = "shit_templates",
                          choices = c(shit_template_names, names(react$templates)))
        updateTextInput(session,
                        inputId = "shit_templateName",
                        value = "")
      }
    } else {
      print("save_shit")
      add_defecation()
      updateTextAreaInput(session,inputId = "shit_comment",value = "")
    }
    if(show.observeEvents)print("#---- observeEvent: input$shit_save --- END")
  })
  
  observeEvent(input$template_confirm_override,{
    if(show.observeEvents)print("#---- observeEvent: input$template_confirm_override --- START")
    add_template()
    add_defecation()
    updateSelectInput(session,
                      inputId = "shit_templates",
                      choices = c(shit_template_names, names(react$templates)))
    updateTextInput(session,
                    inputId = "shit_templateName",
                    value = "")
    removeModal()
    if(show.observeEvents)print("#---- observeEvent: input$template_confirm_override --- END")
  })
  # ################################################################################## food Box ####
  # ---------------------------------------- observeEvent input$food_templates ----
  old_food_selected = c()
  old_food_choices = sort(food_default)  
  idx <- length(old_food_choices)

  observeEvent(input$food_templates, {
    req(!identical(old_food_selected, input$food_templates))
    if(show.observeEvents)print("#---- observeEvent: input$food_templates --- START")
    
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
    if(show.observeEvents)print("#---- observeEvent: input$food_templates --- END")
  }, ignoreNULL = FALSE)
  # ---------------------------------------- observeEvent input$food_add ----
  observeEvent(input$food_add,{
    if(show.observeEvents)print("#---- observeEvent: input$food_add --- START")
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
    if(show.observeEvents)print("#---- observeEvent: input$food_add --- END")
  })
  # ---------------------------------------- observeEvent input$save_food ----
  observeEvent(input$save_food,{
    if(show.observeEvents)print("#---- observeEvent: input$save_food --- START")
    food_default <<- LETTERS[1:4]
    old_food_selected <<- c()
    old_food_choices <<- sort(food_default)  
    idx <-- length(old_food_choices)
    
    # look at templates shit save
    # add_food()
    updateSelectInput(session,"food_templates",choices = food_default)
    removeModal()
    if(show.observeEvents)print("#---- observeEvent: input$save_food --- END")
  })
  # ################################################################################## table Box ####
  # ---------------------------------------- renderDataTable output$shitlist_items ----
  output$shitlist_items <- DT::renderDataTable({
    req(length(react$shitlist)>0)
    out <- map_dfr(react$shitlist, ~as_data_frame(t(.)))
    print(class(out))
    print(names(out))
    out <- DT::datatable(out,
                         selection = "multiple",
                         extensions = 'Scroller',
                         options = list(
                           scrollX = TRUE,
                           scrollY = 300, scroller = TRUE,
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
  # ---------------------------------------- observeEvent input$shitlist_items_cell_clicked ----
  observeEvent(input$shitlist_items_cell_clicked,{
    if(show.observeEvents)print("#---- observeEvent: input$shitlist_items_cell_clicked --- START")
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
    if(show.observeEvents)print("#---- observeEvent: input$shitlist_items_cell_clicked --- END")
  })
  # ---------------------------------------- observeEvent input$shitlist_remove ----
  observeEvent(input$shitlist_remove, {
    if(show.observeEvents)print("#---- observeEvent: input$shitlist_remove --- START")
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
    if(show.observeEvents)print("#---- observeEvent: input$shitlist_remove --- END")
  })
  # ---------------------------------------- observeEvent input$shitlist_confirm_remove ----
  observeEvent(input$shitlist_confirm_remove,{
    if(show.observeEvents)print("#---- observeEvent: input$shitlist_confirm_remove --- START")
    react$shitlist[input$shitlist_items_rows_selected] <- NULL
    removeModal()
    if(show.observeEvents)print("#---- observeEvent: input$shitlist_confirm_remove --- END")
  })
  # ---------------------------------------- observeEvent input$shitlist_edit ----
  observeEvent(input$shitlist_edit, {
    if(show.observeEvents)print("#---- observeEvent: input$shitlist_edit --- START")
    # open shitlist item
    # and edit!!
    # update save_button
    print("editButtonClicked")
    util$edit <- TRUE
    updateActionButton(session,
                       inputId = "shit_save",
                       label = "Override selected")
    updateShitInput(react$shitlist[[input$shitlist_items_rows_selected]])
    if(show.observeEvents)print("#---- observeEvent: input$shitlist_edit --- END")
  })
  
  # ################################################################################## control Box ####
  observeEvent(input$showShitlist,{
    if(show.observeEvents)print("#---- observeEvent: input$showShitlist --- START")
      
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
    if(show.observeEvents)print("#---- observeEvent: input$showShitlist --- END")
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
