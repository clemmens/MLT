DCM <- R6::R6Class(classname="DCM",                  
                   lock_objects = FALSE,
                   public=list(
                     dt = NULL,
                     # Cleaning
                     column_name = NULL,
                     selected_column_cleaning = NULL,
                     all_cleaning_options= NULL,
                     column_remove = NULL,
                     column_type = NULL,
                     column_gsub = NULL,
                     gsub_replacement = NULL,
                     gsub_value = NULL,
                     set_first_row_as_colnames = NULL,
                     initialize = function(default=T, dt) {
                       self$dt <- dt
                       private$history <- list()
                       private$history_input <- list()
                       private$history_error <- list()
                       private$error_massage <- list()
                       private$versions <- 1
                       private$latest_change=Sys.time()
                       # inputs
                       self$set_first_row_as_colnames <- F
                       self$column_remove <- F
                       self$column_gsub <- F
                       self$column_type <- ""
                       self$gsub_replacement <- ""
                       self$gsub_value <- "example"
                       self$all_cleaning_options <- c("set_first_row_as_colnames","column_remove","column_gsub","column_type","selected_column_cleaning","column_name","gsub_replacement","gsub_value")
                     },
                     check_error = function(){
                       if(length(private$error_massage)>0){
                         private$history_error <- list.append(private$history_error,private$error_massage)
                         temp <- private$error_massage 
                         private$error_massage <- list()
                         return(print(temp))
                       }else{
                         private$error_massage <- list()
                         return("succes")
                       }
                     },
                     clean_data = function(){
                       if (!is.null(self$column_name) && !is.null(self$selected_column_cleaning)){
                         private$backup_data()
                         private$save_inputs()
                         if (self$set_first_row_as_colnames==T){
                           tryCatch(names(self$dt) <- as.character(self$dt[1,]),
                                    error=function(e) private$error_massage=list.append(private$error_massage,"First row can't be set as column names"))
                           try(selfdt <- self$dt[-1,])
                           self$set_first_row_as_colnames <- F
                         }
                         if (self$column_remove==T){
                           tryCatch( self$dt[,(self$selected_column_cleaning):=NULL],
                                     error=function(e) private$error_massage=list.append(private$error_massage,paste0("Column ",self$selected_column_cleaning, " can't be removed")))
                           self$column_remove <- F
                         }else{
                           # Rename col names
                           if (self$column_name!=self$selected_column_cleaning){
                             self$dt <- easy_transformations(dt = self$dt, input_column_names = names(self$dt),output_column_names = c(self$column_name,setdiff(names(self$dt),self$selected_column_cleaning)))
                           }
                           if(self$column_type=="character"){x
                             tryCatch(self$dt <-  easy_transformations(dt = self$dt, to_char = self$column_name ),
                                      error=function(e) private$error_massage=list.append(private$error_massage,paste0("Column ",self$column_name, " can't be casted to character"))
                             )
                             self$column_type <- ""
                           }
                           if(self$column_type=="numeric" || self$column_type=="integer"){
                             tryCatch(self$dt <- easy_transformations(dt = self$dt,   to_numeric = self$column_name),
                                      error=function(e) private$error_massage=list.append(private$error_massage,paste0("Column ",self$column_name, " can't be casted to numeric"))
                             )
                             self$column_type <- ""
                           }
                           if(self$column_type=="factor"){
                             tryCatch(self$dt <- easy_transformations(dt = self$dt, to_factor = self$column_name ),                       
                                      error=function(e) private$error_massage=list.append(private$error_massage,paste0("Column ",self$column_name, " can't be casted to factor"))

                             )
                             self$column_type <- ""
                           }
                           if(self$column_gsub==T){
                             tryCatch(self$dt <-  easy_transformations(dt = self$dt,  gsub_column  = self$column_name, gsub_replacement = self$gsub_replacement ,gsub_pattern = self$gsub_value),                                
                                      error=function(e) private$error_massage=list.append(private$error_massage,paste0("Column ",self$column_name, " can't be gsubed"))
                             )
                             self$column_gsub <- F
                           }
                         }
                       }else{
                         private$error_massage=list.append(private$error_massage,"column name missing")
                       }
                       self$check_error() 
                     },
                     revert_data = function(){
                       if (private$versions>1){
                         private$latest_change=Sys.time()
                         private$versions <- private$versions-1
                         self$dt <- private$history[[private$versions]]
                         private$history_input[[private$versions]] <- NULL
                         private$history[[private$versions]] <- NULL
                       }else{
                         private$error_massage <- "Initial version can't be reverted"
                         self$check_error()
                       }
                     }
                   ),# Public
                   private =list(
                     versions= NULL,
                     latest_change =NULL,
                     # Save the history
                     history=NULL,
                     history_input = NULL,
                     history_error = NULL,
                     # Error
                     error_massage = NULL,
                     backup_data = function(){
                       private$latest_change=Sys.time()
                       private$history <- list.append(private$history,copy(self$dt))
                       private$versions <- private$versions+1
                     },
                     save_inputs = function(){
                       private$history_input<- list.append(private$history_input,data.table("set_first_row_as_colnames"=self$set_first_row_as_colnames,
                                                                                           "column_remove"=self$column_remove, 
                                                                                           "column_gsub"=self$column_gsub, 
                                                                                           "column_type"=self$column_type, 
                                                                                           "selected_column_cleaning"=self$selected_column_cleaning, 
                                                                                           "column_name"=self$column_name, 
                                                                                           "gsub_replacement"=self$gsub_replacement, 
                                                                                           "gsub_value"= self$gsub_value
                                                                                           ))
                     }
                   )
                   )# Class
