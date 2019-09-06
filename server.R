cat("Sourcing server.R\n")
source("lib_v01_20180725.R")
source("DCM_class_v01_20190824.R")
source("ML_class_v01_20190712.R")

# global config list
global_cfg = list()
global_cfg$savelog = F 

downloadObjUI <- function(id) {
  ns <- NS(id)
  
  downloadButton(ns("data_download"), label = "Download data", class = "btn-primary")
}

shinyServer(
  function(input, output, session) {
    # SETTINGS FOR SERVER -----------------
    options(shiny.maxRequestSize    = 1000*1024^2,testmode = global_cfg$testmode)
    session$userData$modeldata      = list()
    session$userData$config         = global_cfg
    
    # within session globally available variables
    global_reactive                 = reactiveValues()
    
    output$EARL_body <- renderUI({earl_app_body(session)})
    output$EARL_sidebar <- renderMenu({earl_menu()})
    
    # Observe events
    
    downloadObj <- function(input, output, session, data,extension) {
      
      output$data_download <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), extension, sep="")
        },
        content = function(file) {
          if (extension==".csv"){
            write.csv(data(), file) 
          } # add parentheses to data arg if reactive
          if (extension==".rds"){
            saveRDS(data(), file) 
          }
          if(extension==".feater"){
            write_feather(data(), file) 
          }
          if(extension==".pdf"){
            pdf(file)
            session$userData$modeldata$ML_class$results()
            dev.off()
            contentType = "application/pdf"
          }
        }
      )
    }
    callModule(downloadObj, id = "download1",extension=".csv", data = reactive(session$userData$modeldata$ML_class$dt))
    callModule(downloadObj, id = "download2",extension=".rds", data = reactive(session$userData$modeldata$ML_class$dt))
    callModule(downloadObj, id = "download3",extension=".feater", data = reactive(session$userData$modeldata$ML_class$dt))
    callModule(downloadObj, id = "download5",extension=".rds", data = reactive(session$userData$modeldata$DCM_class))
    callModule(downloadObj, id = "download6",extension=".csv", data = reactive(session$userData$modeldata$ML_class$dt_result))
    callModule(downloadObj, id = "download7",extension=".rds", data = reactive(session$userData$modeldata$ML_class$dt_result))
    callModule(downloadObj, id = "download8",extension=".feater", data = reactive(session$userData$modeldata$ML_class$dt_result))
    callModule(downloadObj, id = "download9",extension=".rds", data = reactive(session$userData$modeldata$ML_class))
    callModule(downloadObj, id = "download10",extension= ".pdf",data=reactive(session$userData$modeldata$ML_class$results()) )
    
    
    # cleaning
    observeEvent(input$selected_column_cleaning,{
      updateTextInput(session = session,inputId = "column_name",value = input$selected_column_cleaning)
      updateSelectInput(session = session,inputId = "column_type",selected = class(session$userData$modeldata$ML_class$dt[,(input$selected_column_cleaning),with=F][[1]]))
    })
    
    observeEvent(input$select_table,{
      if (input$select_table!=""){
        if (input$select_table=="Data outliers" || input$select_table=="Data distribution" || input$select_table=="Categorical distributions"){
          output$dt_table_2 <- NULL
          tt <- session$userData$modeldata$friendly_output_names[friendly_table_names==input$select_table,table_names]
          if (input$select_table=="Data outliers"){
            output$dt_graph_2 <- renderPlot(ggplot(stack(
              session$userData$modeldata$ML_class$dt[,(session$userData$modeldata$ML_class$dt_clas[class=="numeric",col]),with=F]),aes(ind,values))+geom_boxplot())
          }
          if (input$select_table=="Data distribution"){
            output$dt_graph_2 <- renderPlot(ggplot(stack(
              session$userData$modeldata$ML_class$dt[,(session$userData$modeldata$ML_class$dt_clas[class=="numeric",col]),with=F]),aes(values)) + facet_wrap(~ ind, scales = "free") + geom_density())
          }
          if (input$select_table=="Categorical distributions"){
            output$dt_graph_2 = renderPlot(session$userData$modeldata$ML_class$dt[,(session$userData$modeldata$ML_class$dt_clas[class=="factor",col]),with=F] %>%
                                                   # gather to long format
                                                   gather(na.rm = TRUE) %>%
                                                   # get the frequency count of key, value columns
                                                   count(key, value) %>% 
                                                   ggplot(., aes(x = value, y = n)) + 
                                                   geom_bar(stat = "identity") + 
                                                   # facet wrap with key column
                                                   facet_wrap(~ key)) 
          }
          #output$dt_graph_2 <- renderPlot(expr = session$userData$modeldata$ML_class[[tt]])
          
        }else{
          output$dt_graph_2 <- NULL
          tt <- session$userData$modeldata$friendly_output_names[friendly_table_names==input$select_table,table_names]
          output$dt_table_2 <- renderDataTable(expr = session$userData$modeldata$ML_class[[tt]],options = list(lengthChange = FALSE,pageLength = 100,paging = F,scrollX=T,scrollY = "60vh", sDom  = "<'top'>lrt<'bottom'>p",searchHighlight = TRUE,searching = F)) 
        }
      }
    })
    
    observeEvent(input$execute_cleaning,{
      if(!is.null(session$userData$modeldata$ML_class$dt)){
        if(!is.na(session$userData$modeldata$ML_class$dt)){
          nm_input <- session$userData$modeldata$DCM_class$all_cleaning_options
          for ( i in 1: length(nm_input)){
            session$userData$modeldata$DCM[[nm_input[i]]] <-  input[[nm_input[i]]]
          }
          session$userData$modeldata$DCM_class$clean_data()
          output$cleaning_status <- renderText(session$userData$modeldata$DCM_class$check_error()) 
          session$userData$modeldata$ML_class$dt <- session$userData$modeldata$DCM_class$dt
          set_inputs_to_initial_value(input,output,session)
          update_app_inputs(input,output,session)
          update_app_output(input,output,session)
        }else{
          shinyalert(title = "Data needed",text = "Upload data first",type = "warning")
        }
      }else{
        shinyalert(title = "Data needed",text = "Upload data first",type = "warning")
      }
    })
    
    set_inputs_to_initial_value <- function(input,output,session){
      updateMaterialSwitch(session = session,inputId = "set_first_row_as_colnames",value = F)
      updateMaterialSwitch(session = session,inputId = "column_remove",value = F)
      updateMaterialSwitch(session = session,inputId = "column_gsub",value = F)
      updateTextInput(session = session,inputId = "gsub_value",value = NA)
      updateTextInput(session = session,inputId = "gsub_replacement",value = "")
    }
    
    observeEvent(input$revert_cleaning,{
      if(!is.null(session$userData$modeldata$ML_class$dt)){
        if(!is.na(session$userData$modeldata$ML_class$dt)){
          session$userData$modeldata$DCM_class$revert_data()
          session$userData$modeldata$ML_class$dt <- as.data.table(session$userData$modeldata$DCM_class$dt)
          update_app_inputs(input,output,session)
          update_app_output(input,output,session)
        }else{
          shinyalert(title = "Data needed",text = "Upload data first",type = "warning")
        }
      }else{
        shinyalert(title = "Data needed",text = "Upload data first",type = "warning")
      }
    })
      
    ## run ML 
    observeEvent(input$run_ml,{
      if(!is.null(session$userData$modeldata$ML_class$dt)){
        if(!is.na(session$userData$modeldata$ML_class$dt)){
          progress <- shiny::Progress$new()
          progress$set(message = "Running algorithm", value = 0)
          # Process user inputs
          set_user_input(input,output,session)
          #Run main ML
          session$userData$modeldata$ML_class$run_date <- Sys.time()
          session$userData$modeldata$ML_class$owner <- input$owner
          tryCatch(session$userData$modeldata$ML_class$main_ML(), 
                   error=function(e) shinyalert(title = "Error",text = "A critical error has occured",type = "error"))
          #try(session$userData$modeldata$ML_class$main_ML())
          progress$set(message = "Algorithm finished", value = 1)
          on.exit(progress$close())
          #browser()
          output$dt_graph_3 <- renderPlot(expr = session$userData$modeldata$ML_class$results())
          #output$key_indicators <- 
        }else{
          shinyalert(title = "Data needed",text = "Upload data first",type = "warning")
        }
      }else{
        shinyalert(title = "Data needed",text = "Upload data first",type = "warning")
      }
    })
    
    observeEvent(input$task,{
      if (input$task=="classification"){
        updateSelectInput(session = session,inputId = "analysis",choices = c("tree","knn","xgbTree"))
      }
      if (input$task=="clustering"){
        updateSelectInput(session = session,inputId = "analysis",choices = c("kmeans","clara"))
      }
      if (input$task=="regression"){
        updateSelectInput(session = session,inputId = "analysis",choices = c("linear"))
      }
    })
    
    observeEvent(input$load_data,{
      dt <- loadData(input,output,session)
      #browser()
      if(is.na(dt)){
        shinyalert("Error", "File type not supported", type = "error")
      }else{
        output$table <- renderTable({dt[1:18,]},rownames = F) 
        output$dt_table <- renderDataTable(expr = dt,options = list(lengthChange = FALSE,pageLength = 100,paging = T,scrollX=T,scrollY = "60vh", sDom  = "<'top'>lrt<'bottom'>p",searchHighlight = TRUE,searching = T))
        # create ML_class
        session$userData$modeldata$ML_class = ML$new(dt=dt)
        session$userData$modeldata$DCM_class = DCM$new(dt=dt)
        session$userData$modeldata$ML_class$filename <- input$files$datapath
        session$userData$modeldata$ML_class$shiny_app <- T
        updateNumericInput(session = session,inputId = "minbucket",value = floor(nrow(session$userData$modeldata$ML_class$dt)/10))
        update_app_inputs(input,output,session)
        update_app_output(input,output,session)
      }
    })
  })

earl_app_body <- function(session) {
  div(class = "div-full", 
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
      useShinyalert(),
      tabItems(
        tabItem(tabName = "home_tab", class = "active",
                h1('Welkom to the Machine learning asset'),
                h5('More text to follow'),
                # include css stylesheet
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "style_main.css")
                )
        ),
        tabItem(tabName = "upload_tab",
                tabsetPanel(id = "upload_tabsetPanel",
                            tabPanel(title = "Upload data",
                  h3('Upload files'),
                  fileInput("files", NA, multiple = FALSE, accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv",".xlsx",".xls",".rds",".feather") ),
                  textInput(inputId = "owner",label = NA,placeholder = "Name of person running the application",value = ""),
                  br(),
                  actionButton(inputId = "load_data",label = "Load data in application"),
                  br(),
                  br(),
                  tableOutput("table"),
                  # include css stylesheet
                  tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                    tags$link(rel = "stylesheet", type = "text/css", href = "style_main.css")
                  )
                  ),
                  tabPanel(title = "Clean data",
                           h3(" Limited cleaning of data"),
                           sidebarLayout(
                             sidebarPanel(
                               materialSwitch(inputId = "set_first_row_as_colnames",label = "Use first row as names",value = F,status = "success"),
                               tags$hr(),
                               h4("Clean columns"),
                               selectInput(inputId = "selected_column_cleaning",label = "Choose column",choices = ""),
                               materialSwitch(inputId = "column_remove",label = "Remove column",value = F,status = "success"),
                               conditionalPanel(
                                 condition = "input.column_remove==false",
                                 textInput(inputId = "column_name",label = "Column name"),
                                 selectInput(inputId = "column_type",label = "Column type",choices = c("numeric","factor","character","integer")),
                                 materialSwitch(inputId = "column_gsub",label = "Replace part of string",value = F,status = "success"),
                                 conditionalPanel(
                                   condition = "input.column_gsub==true",
                                   textInput(inputId = "gsub_value",label = "String to replace"),
                                   textInput(inputId = "gsub_replacement",label = "Replacements",value = "")
                                 )
                                 ),
                               tags$hr(),
                               div(actionButton(inputId = "revert_cleaning",label = "Revert",icon = icon("fas fa-history")),align = "center",
                                   actionButton(inputId = "execute_cleaning",label = "Excecute",icon = icon("fas fa-check"))),
                               conditionalPanel("input.execute_cleaning>=1",
                               tags$hr(),
                               div(h4(textOutput("cleaning_status")),align="center")
                               )
                             ),
                             #textOutput("cleaning_status"),
                             
                             mainPanel(
                               column(10,
                               dataTableOutput("dt_table")
                               )
                             )
                           )
                           ),
                  tabPanel(title = "Inspect data",
                           h3("Check data quality"),
                           sidebarLayout(
                             sidebarPanel(
                               h4("Select result to display"),
                               selectInput(inputId = "select_table",label = "Select table",choices = "")
                             ),
                             mainPanel(
                               column(10,
                                      dataTableOutput("dt_table_2"),
                                      plotOutput("dt_graph_2")
                               )
                                       )
                           )
                           )
                )
        ),
        tabItem(tabName = "settings_tab",
                tabsetPanel(id = "setting_tabsetPanel",
                            tabPanel("General settings",
                                     br(),
                                     selectInput(inputId = "task",label = h3("Choose task to perform"),choices = c("clustering","classification","regression"),multiple = F),
                                     br(),
                                     selectInput(inputId = "analysis",label = h3("Choose algorithm to apply"),choices = c("")),
                                     conditionalPanel(
                                       condition = "input.task == 'classification'",
                                       selectInput(inputId = "target",label = "Target column",choices = "")
                                    )
                            ),
                            tabPanel("Detailed settings",
                                     br(),
                                     h4('Overall settings'),
                                     materialSwitch(inputId = "detail_info", label = "Show detailed results",value = F,status = "success"),
                                     br(),
                                     conditionalPanel(
                                       condition = "input.analysis == 'kmeans' || input.analysis == 'clara' ",
                                       h4("Clustering detailed settings"),
                                       materialSwitch(inputId = "auto_det_k",label = "Auto set number of clusters",status = "success",value = T),
                                       conditionalPanel(
                                         condition = "input.auto_det_k == false",
                                         numericInput(inputId = "k_kmeans",label = "Number clusters",min = 1,value = 3)
                                       )
                                     ),
                                     br(),
                                     conditionalPanel(
                                       condition = "input.task == 'classification'",
                                       h4('Feature selection detailed settings'),
                                       materialSwitch(inputId = "feat_sel_bool",label = "Perform feature selection",status = "success",value = F),
                                       conditionalPanel(
                                         condition = "input.feat_sel_bool==true",
                                         selectInput(inputId = "feat_sel_always_keep_cols",label = "Columns not to throw out during feature selection",choices = "",multiple = T)
                                       )
                                     )
                                     ),
                            tabPanel("Specific settings",
                                     fluidRow(
                                       column(6,
                                     br(),
                                     h4("Overall settings"),
                                     numericInput(inputId = "seed",label = "seed",min = 0,value = 123),
                                     numericInput(inputId = "cast_treshold",label = "Upper treshold autocast to factor",value = 5,min = 1),
                                     br(),
                                     conditionalPanel(
                                       condition = "input.analysis == 'tree'",
                                        h4("Tree specific settings"),
                                        materialSwitch(inputId = "auto_prune_bool",label = "Auto prune tree",status = "success",value = T),
                                        numericInput(inputId = "xval",label = "Number cross validations",min = 5,value = 20),
                                        numericInput(inputId = "maxdepth",label = "Maximum dept three",value = 6,min = 1),
                                        numericInput(inputId = "minbucket",label = "Minumum size bucket ±10% of nrows",min = 1,value = 1)
                                     ),
                                     conditionalPanel(
                                       condition = "input.analysis == 'knn'",
                                       h4("KNN specific settings"),
                                       numericInput(inputId = "number_repeatedcv",label = "Number cross validations",min = 5,value = 20),
                                       numericInput(inputId = "repeats_repeatedcv",label = "Number of repeats validations",min = 5,value = 3),
                                       #selectInput(inputId = "metric",label = "Evaluation metric",choices = c("Accuracy","ROC","Kappa"),multiple = F),
                                       selectInput(inputId = "preProcess_knn",label = "Auto preprocess steps",choices = c("center","scale"),multiple = T,selected = c("center","scale"))
                                     ),
                                     conditionalPanel(
                                       condition = "input.analysis == 'clara'",
                                       h4("Clara specific settings"),
                                       materialSwitch(inputId = "clara_standardize",label = "Auto scale and center",status = "success",value = T)
                                     )
                                       ),#column
                                     column(6,
                                     br(),
                                     conditionalPanel(
                                       condition = "input.task == 'classification' && input.feat_sel_bool == true",
                                       h4('Feature selection specific settings'),
                                       radioButtons(inputId = "RF",label = "Feature selection algorithm",choices = "RF",selected = "RF"),
                                       materialSwitch(inputId = "feat_sel_rem_low_var",label = "Remove columns with low variance",value = T,status = "success"),
                                       materialSwitch(inputId = "feat_sel_rem_corr",label = "Remove columns with high correlation",value = T,status = "success"),
                                       numericInput(inputId = "feat_sel_rem_corr_cutoff",label = "Maximum allowed correlation between features",min = 0,max = 1,value = 0.6),
                                       numericInput(inputId = "feat_sel_cutoff",label = "Treshold for feature selection",value = 50,min = 1,max = 100)
                                     )
                                     )
                            )#fluidrow
                                     )# Specialistic tab
                ),
                br(),
                h3('Run machine learning algorithm'),
                actionButton(inputId = "run_ml",label = " Run machine learning algorithm",icon = icon("fas fa-angle-double-right")),
                # include css stylesheet
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "style_main.css")
                )
        ),
        tabItem(tabName = "results_tab",
                conditionalPanel(condition = "input.run_ml==0",h3("Run machine learning algorithm first")),
                conditionalPanel(condition = "input.run_ml!=0",
                                 h3(" Inspect ML results"),
                                 br(),
                                 fluidRow(
                                   column(10,
                                          #dataTableOutput("dt_table_2"),
                                          plotOutput("dt_graph_3")
                                   )
                                 )
                                 )
                
                ),
        tabItem(tabName = "download_tab",
                conditionalPanel(condition = "input.load_data!=0",
                                 br(),
                                 fluidRow(
                                   column(4,
                                          h3("Download input"),
                                          h5("Csv file"),
                                          downloadObjUI(id = "download1"),
                                          h5("R file"),
                                          downloadObjUI(id = "download2"),
                                          h5("Python file"),
                                          downloadObjUI(id = "download3")
                                   ),
                                   column(4,
                                          h3("Download cleaning report"),
                                          h5("Report PDF file"),
                                          downloadObjUI(id = "download4"),
                                          h5("DCM  R flie"),
                                          downloadObjUI(id = "download5")
                                          
                                   )
                                 )
                                 ), #conditionalpanel
                conditionalPanel(condition = "input.load_data==0",h3("Load data first")),
                conditionalPanel("input.run_ml!=0",
                                 br(),
                                 fluidRow(
                                   column(4,
                                          h3("Download results"),
                                          h5("Csv file"),
                                          downloadObjUI(id = "download6"),
                                          h5("R file"),
                                          downloadObjUI(id = "download7"),
                                          h5("Python file"),
                                          downloadObjUI(id = "download8")
                                   ),
                                   column(4,
                                          h3("Download model & graphs"),
                                          h5("ML model R file"),
                                          downloadObjUI(id = "download9"),
                                          h5("Graphs PDF file"),
                                          downloadObjUI("download10")
                                          
                                          )
                                 )
   
                ),
                
                # include css stylesheet
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "style_main.css")
                )
        )
      )
  )
  #browser()
  #if()
  #updateSelectInput(session = session,inputId = "analysis",selected ="hoi" )
}

earl_menu = function() {
  sidebarMenu(id = "sidebar_menu",
              menuItem('Home', icon = icon('home'), tabName = 'home_tab'),
              menuItem('Upload data', icon = icon('fal fa-cloud-upload'), tabName = 'upload_tab'),
              menuItem('Set parameters',icon=icon('fal fa-sliders'),tabName = "settings_tab"),
              menuItem("Show results",icon=icon('fas fa-search-plus'),tabName = "results_tab"),
              menuItem('Download results', icon = icon('fal fa-cloud-download'), tabName = 'download_tab')
  )
}


loadData = function(input,output,session){  
  if( !is.null(input$files$datapath)){
    file_extension = tail(as.vector(str_split(input$files$datapath,pattern = "/."))[[1]],n=1)
    dt <- NA
    if (file_extension==".xlsx"){
      try(dt <- as.data.table(read_xlsx(path =input$files$datapath )))
    }#xlsx
    if (file_extension==".feather"){
      try(dt <- as.data.table(read_feather(path =input$files$datapath )))
    }#feather
    if (file_extension==".xls"){
      try(dt <- as.data.table(read_xls(path =input$files$datapath )))
    }#xls
    if (file_extension==".rds"){
      try(dt <- as.data.table(readRDS(file =input$files$datapath )))
    }#rds
    if(file_extension==".csv"){
      try(temp <- as.data.table(read.csv(file =input$files$datapath,sep = ";" )))
      try(temp2 <- as.data.table(read.csv(file =input$files$datapath,sep = "," )))
      if(length(names(temp))>length(names(temp2))){
        dt = temp
      }else{
        dt = temp2
      }
    }#csv
    if (file_extension %ni% c(".rds",".xlsx",".xls",".csv")){
      dt <- NA
      cat("file type not supported \n")
    }
    return(dt)
  }else{
    return(NA)
  }
}

set_user_input = function(input, output, session){
  nm_input <- setdiff(names(reactiveValuesToList(input)),
                      c("files","selected_column_cleaning","cleaning_panels","gsub_value","column_name","column_remove",
                        "load_data","gsub_replacement","column_type-selectized","sidebarItemExpanded","olumn_gsub","setting_tabsetPanel","run_ml","column_type",
                        "selected_column_cleaning-selectized","upload_tabsetPanel","sidebarCollapsed","set_first_row_as_colnames","execute_cleaning","revert_cleaning",
                        "target-selectized","select_graph","sidebar_menu","select_table","column_gsub","select_graph","feat_sel_always_keep_cols-selectized",
                        "task-selectized","preProcess_knn-selectized","select_table-selectized","analysis-selectized","metric-selectized"))
  #browser()
  
  for (i in 1:length(nm_input)){
    #if (!is.null(session$userData$modeldata$ML_class[[nm_input[i]]] )){
      session$userData$modeldata$ML_class[[nm_input[i]]] <-  input[[nm_input[i]]]
    #}else{
      cat(paste("Try to include these parameters ",nm_input[i]),"\n")
    #}
  }
}

update_app_inputs <- function(input,output,session){
  updateSelectInput(session = session,inputId = "feat_sel_always_keep_cols",choices = names(session$userData$modeldata$ML_class$dt))
  updateSelectInput(session = session,inputId = "target",choices = names(session$userData$modeldata$ML_class$dt))
  output$dt_table <- renderDataTable(expr = session$userData$modeldata$ML_class$dt,options = list(lengthChange = FALSE,pageLength = 100,paging = T,scrollX=T,scrollY = "60vh", sDom  = "<'top'>lrt<'bottom'>p",searchHighlight = TRUE,searching = T))
  updateSelectInput(session = session,inputId = "selected_column_cleaning",choices =names(session$userData$modeldata$ML_class$dt) ) 
}

update_app_output = function(input,output,session){
  friendly_table_names = c("Data types","Unique data values","Missing data","Data outliers","Data distribution","Categorical distributions")
  table_names = c("dt_clas","unique_values","na_overview","plot_numeric","plot_factor","categorical_distributions")
  session$userData$modeldata$friendly_output_names <- cbind(as.data.table(friendly_table_names),as.data.table(table_names))
  updateSelectInput(session = session,inputId = "select_table",choices = c(friendly_table_names))
  session$userData$modeldata$ML_class$check_data_quality()
}


