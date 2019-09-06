#################################################################################################x
# Author(s)     : Chris Lemmens
# Description   : Collection of helper functions
# 
# STATUS        : DEVELOPMENT
#################################################################################################x
# CHANGLOG:
# v01 - 20170829 - CLE
#   + Initial version 
##################################################################################################x

# Init libs ----
library(R6)
library(dplyr) # Data munging
library(ggplot2) # fancy plotting
library(tidyr) # Helper functions
library(stringr) # String manipulations
library(readxl) # Read xls
library(data.table) # Fast data munging
library(zoo) #na.approx()
library(gridExtra) # arrange multiple plots
library(rprojroot) # Rroot
library(lubridate) # Date handling
library(caret) # Machine learning
library(ROCR)
#library(xlsx)
library(boot)
library(rpart)
library(rpart.plot)
library(corrplot)
library(ggalt)
library(factoextra)
library(FactoMineR)
library(factoextra)
library(NbClust)
library(C50)
library(mlbench)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(shinyBS)
library(feather)
library(rlist)
# Define operators ----

# Are you not also tired of writing !(a %in% b)? Use a %ni% b
`%ni%` <- Negate(`%in%`)
# Example:
# c("a", "b", "c") %in% c("c", "d", "e")
# c("a", "b", "c") %ni% c("c", "d", "e")

# Define functions ----


# ================================================================================================x
# Description     : Add cluster as col to dt
# Input           : -
# Output          : -
# Review status   : DEV
# ================================================================================================x
add_cluster_col <- function(dt, tree, digits=0) {
  dt$tree_class_value <- predict(tree, dt)
  levels <- unique(dt$tree_class_value)
  levels <- levels[order(levels)]
  labels <- round(levels, digits)
  labels <- paste0(LETTERS[1:length(labels)], ") ", labels)
  dt[, cluster:=factor(tree_class_value, levels=levels, labels=labels)]
}


# ================================================================================================x
# Description     : Inverse of which function. Numeric index to logical index.
# Input           : -
# Output          : -
# Review status   : DEV
# ================================================================================================x
which_logical <- function(indices, totlength) is.element(seq_len(totlength), indices)


# ================================================================================================x
# Description     : Create interpolated column by store using zero order hold
# Input           : -
# Output          : -
# Review status   : DEV
# ================================================================================================x
interpolate_store_zoh <- function(dt, col, CC_prefix="CC_", CC_suffix=""){
  # Debug: col="nfs"
  # Order dt
  dt <- dt[order(store, CC_date)]
  # Which stores contain only NA?
  s_na <- dt[, .(rel_amount_na=sum(is.na(get(col)))/.N), by=store][rel_amount_na==1, store] 
  # Interpolate backward
  dt[store %ni% s_na, 
     paste0(CC_prefix, col, CC_suffix):=na.locf(get(col), fromLast=T, na.rm=F, rule=2),
     by=store]
  # Interpolate forward
  dt[store %ni% s_na, 
     paste0(CC_prefix, col, CC_suffix):=na.locf(get(paste0(CC_prefix, col, CC_suffix)), fromLast=F, na.rm=F, rule=2),
     by=store]
  return(dt)
}




# ================================================================================================x
# Description     : returns the vector scaled with new name
# Input           : -
# Output          : -
# Review status   : DEV
# QC JLI          : What is the intended application for this function?
# ================================================================================================x
scaler <- function(x) {
  x <- (x-min(x, na.rm=T)) / (max(x, na.rm=T)-min(x, na.rm=T))
  names(x) <- paste0("scaled_", colnames(x))
  return(x)
}

# ================================================================================================x
# Description     : add scaled variables to existing df
# Input           : -
# Output          : -
# Review status   : DEV
# QC JLI          : What is the intended application for this function?
# ================================================================================================x
add_scaled_columns <- function(df, col_list) {
  for(col in col_list) {
    tmp <- select(df, contains(col))
    tmp <- scaler(tmp)
    df <- cbind(df, tmp)
  }
  return(df)
}

# ================================================================================================x
# Description     : set outliers on the left to min 
# Input           : -
# Output          : -
# Review status   : DEV
# QC JLI          : What is the intended application for this function?
# ================================================================================================x
remove_outliers_left <- function(x, na.rm = TRUE,probs=.025) {
  qnt <- quantile(x, probs=probs, na.rm = na.rm)
  mn <- min(x,na.rm=na.rm)
  x[x < qnt] <- mn
  return(x)
}

# ================================================================================================x
# Description     : set outliers on the right to max 
# Input           : -
# Output          : -
# Review status   : DEV
# QC JLI          : What is the intended application for this function?
# ================================================================================================x
remove_outliers_right <- function(x, na.rm = TRUE,probs=.975) {
  qnt <- quantile(x, probs=probs, na.rm = na.rm)
  mx <- max(x,na.rm=na.rm)
  x[x > qnt] <- mx
  return(x)
}

# ================================================================================================x
# Description     : Output number of NA in each column as ordered data table
# Input           : -
# Output          : -
# Review status   : DEV
# ================================================================================================x
inspect_na <- function(dt) {
  na_count <- sapply(dt, function(y) sum(length(which(is.na(y)))))
  inf_count <- sapply(dt, function(y) sum(length(which(is.infinite(y)))))
  na_count <- data.table(col=names(na_count), 
                         n_na=na_count, 
                         n_non_na=nrow(dt)-na_count,
                         n_inf=inf_count,
                         n_fin=nrow(dt)-inf_count )
  na_count <- na_count[order(n_na, decreasing = T)]
  
  return(na_count)
}

# ================================================================================================x
# Description     : Remove NA from data table
# Input           : -
# Output          : -
# Review status   : DEV
# ================================================================================================x
remove_inf = function(dt){
  for (j in 1:ncol(dt)) set(dt, which(is.infinite(dt[[j]])), j, NA)
  return(dt)
}

# ================================================================================================x
# Description     : Perform most common transformations for example casting, na removal, renaming
# Input           : Data table
# Output          : Data table
# Review status   : DEV
# ================================================================================================x
easy_transformations <- function(dt,
                                 input_column_names=colnames(dt), 
                                 output_column_names=colnames(dt), 
                                 gsub_column=NULL, 
                                 gsub_pattern=NULL, 
                                 gsub_replacement="",
                                 substr_column=NULL,
                                 substr_begin=NULL,
                                 substr_end=NULL,
                                 replace_column=NULL,
                                 replace_patern=NULL,
                                 replace_replacement=NULL,
                                 to_char=NULL, 
                                 to_numeric= NULL, 
                                 to_factor=NULL,
                                 remove_na=NULL ) {
  # Rename
  setnames(dt,input_column_names,output_column_names)
  # Substr
  if (is.null(substr_column)==F){
    dt[, (substr_column) := Map(substr,.SD,start = substr_begin,stop = substr_end), .SDcols=substr_column]
  }
  # Gsub
  if (is.null(gsub_column)==F){
    if (gsub_replacement=="" & length(gsub_pattern)>1){gsub_replacement<- rep("",length(gsub_pattern))}
    dt[, (gsub_column) := Map(gsub,.SD,pattern  = gsub_pattern,replacement = gsub_replacement),
       .SDcols=gsub_column]
  }
  # Replace
  if (is.null(replace_column)==F){
    dt[, (replace_column) := Map( replace,.SD,list=replace_patern,values=replace_replacement),
       .SDcols=replace_column]
  }
  # Cast to char
  if (is.null(to_char)==F){
    dt[, (to_char) := lapply(.SD, as.character), .SDcols=to_char]
  }
  # Cast to numeric
  if (is.null(to_numeric)==F){
    dt[, (to_numeric) := lapply(.SD, as.character), .SDcols=to_numeric]
    dt[, (to_numeric) := lapply(.SD, as.numeric), .SDcols=to_numeric]
  }
  if (is.null(to_factor)==F){
    dt[, (to_factor) := lapply(.SD, as.character), .SDcols=to_factor]
    dt[, (to_factor) := lapply(.SD, as.factor), .SDcols=to_factor]
  }
  # Remove NA
  if (is.null(remove_na)==F){
    dt <- dt[complete.cases(dt[,remove_na, with=F]), ]
  }
  return(dt)
}

# ================================================================================================x
# Description     : Adding a new column to dt, discretized version of col with Low, Medium, High
# Input           : -
# Output          : -
# Review status   : DEV
# ================================================================================================x
num_2_sml <- function(dt, col, overwrite=F) {
  tmp <- dt[, c("store", "CC_year", col), with=F]
  names(tmp)[3] <- "val"
  ggplot(tmp) + geom_density(aes(x=val))
  # Compute tertiles for S/M/L
  tmp[, q1:=quantile(val, probs=1/3, na.rm=TRUE)]
  tmp[, q2:=quantile(val, probs=2/3, na.rm=TRUE)]
  # Compute SML
  tmp[val<=q1, val_sml:="Low"]
  tmp[val<=q2 & val>q1, val_sml:="Middle"]
  tmp[val>q2, val_sml:="High"]
  # Check
  ggplot(tmp) + geom_density(aes(x=val, color=val_sml))
  # Overwrite current column
  if(overwrite) {
    col_discretized <-col
    set(dt, j=col, value=tmp$val_sml)
  } else {
    col_discretized <-paste0(col, "_disc")
    setnames(tmp, "val_sml", col_discretized)
    # Join to dt
    dt <- merge(dt, tmp[, c("store", "CC_year", col_discretized), with=F], by=c("store", "CC_year"), all.x=T)
  }
  
  return(dt)
}

# ================================================================================================x
# Description     : Time shift of variables depending on CC_year (get previous year value)
# Input           : -
# Output          : -
# Review status   : DEV
# Status          : Obsolete - use time_shift_gen
# ================================================================================================x
time_shift <- function(dt, cols2shift, lag, joinby=c("store", "CC_year"), time_col="CC_year") {
  # Debug: dt=sy; cols2shift=c("sales_m2_week"); lag=1; joinby=c("store", "CC_year")

  # Shift only ones which are found in data
  cols2shift <- cols2shift[cols2shift %in% names(dt)]
  # Check if shifts already exist and shift only those not existing
  new_col_names <- paste0(cols2shift, "_t", lag)
  cols2shift <- cols2shift[!(new_col_names %in% names(dt))]
  # If nothing to shift
  if (length(cols2shift) == 0) 
    return(dt)
  # Deep copy of dt
  tmp <- as.data.table(as.data.frame(dt[, c(joinby, cols2shift), with=F])) 
  # Shift
  val <- tmp[, get(time_col)] + lag
  set(x = tmp, j = time_col, value = val)
  # Set new names
  setnames(tmp, cols2shift, new_col_names)
  # Join new cols to input table
  dt <- merge(dt, tmp, by=joinby, all.x=T)
  return(dt)
}


# ================================================================================================x
# Description     : Time shift of variables by day, week, month or year
# Input           : -
# Output          : -
# Review status   : DEV
# ================================================================================================x
time_shift_gen <- function(dt, cols2shift, shift_unit="year", shift_by=-1, join_by=c("store"), time_col="CC_date") {
  join_by <- c(join_by, time_col)
  # Determine new col names
  if (shift_by < 0) {
    cols_new <- paste0(cols2shift, "_tm", abs(shift_by)) # t minus x
  } else if (shift_by > 0) {
    cols_new <- paste0(cols2shift, "_tp", abs(shift_by)) # t plus x
  } else {
    stop("Shift zero?")
  }
  # Shift only ones which are found in data
  cols2shift <- cols2shift[cols2shift %in% names(dt)]
  # Check if shifts already exist and shift only those not existing
  cols2shift <- cols2shift[!(cols_new %in% names(dt))]
  # If nothing to shift
  if (length(cols2shift) == 0) 
    return(dt)
  # Deep copy of sub data table
  dt_shift <- as.data.table(as.data.frame(dt[, c(join_by, cols2shift), with=F]))
  # Shift date
  if (shift_unit == "week") {
    week(dt_shift$CC_date) <- week(dt_shift$CC_date) + shift_by
  } else if (shift_unit == "day") {
    day(dt_shift$CC_date) <- day(dt_shift$CC_date) + shift_by
  } else if (shift_unit == "month") {
    month(dt_shift$CC_date) <- month(dt_shift$CC_date) + shift_by
  }else if (shift_unit == "day") {
    year(dt_shift$CC_date) <- year(dt_shift$CC_date) + shift_by
  }
  # Rename columns
  setnames(dt_shift, cols2shift, cols_new)
  # Join
  dt <- merge(dt, dt_shift, by=join_by, all.x=T)
  # Order by join_by
  dt <- dt[order(dt[, get(join_by)])]
  return(dt) 
}

# ================================================================================================x
# Description     : Adding absolute or relative delta to data table for selected columns
# Input           : dt - Data table
#                   col_delta - Vector of column names for which delta to be computed
#                   delta_typ - "absolute" or "relative"
# Output          : dt - Data table
# Review status   : DEV
# ================================================================================================x
delta <- function(dt, col_delta, delta_type, joinby=c("store", "CC_year")){
  dt <- as.data.table(as.data.frame(dt)) # Deep copy
  # Check if col exists
  idx_found <- col_delta %in% names(dt)
  cols_not_found <- col_delta[!idx_found]
  cols_found <- col_delta[idx_found]
  if (length(cols_not_found) > 0) {
    warning("Cannot compute delta. Columns '", paste(cols_not_found, collapse = "', '"), "' not found.")
  }
  # Compute time shift if not exists
  dt <- time_shift(dt, cols_found, lag=1, joinby=joinby)
  # Relative delta
  if (delta_type == "relative") {
    col_new_names <- paste0(cols_found, "_delta_rel")
    # Only create if not already exist
    idx_2_compute <- !(col_new_names %in% names(dt))
    cols_2_compute <- cols_found[idx_2_compute]
    col_new_names <- col_new_names[idx_2_compute]
    # Compute delta for each column
    for (i in 1:length(cols_2_compute)){
      # Column names
      col_t <- cols_2_compute[i]
      col_t1 <- paste0(col_t, "_t1")
      col_new <- col_new_names[i]
      # Compute
      delta <- dt[, get(col_t)/get(col_t1)]
      set(dt, j=col_new, value=delta)
    } # end for all cols
  } else if (delta_type == "absolute") {
    col_new_names <- paste0(cols_found, "_delta_abs")
    # Only create if not already exist
    idx_2_compute <- !(col_new_names %in% names(dt))
    cols_2_compute <- cols_found[idx_2_compute]
    col_new_names <- col_new_names[idx_2_compute]
    # Compute delta for each column
    for (i in length(cols_2_compute)){
      # Column names
      col_t <- cols_2_compute[i]
      col_t1 <- paste0(col_t, "_t1")
      col_new <- col_new_names[i]
      # Compute
      delta <- dt[, get(col_t) - get(col_t1)]
      set(dt, j=col_new, value=delta)
    } # end for all cols
  }
  return(dt)
}

# ================================================================================================x
# Description     : Search dat table header for string
# Input           : dt: Data table 
#                   string: String to be searched for
# Output          : -
# Review status   : DEV
# ================================================================================================x
search.att.name <- function(dt, to_match, fixed=T) {
  col_names <- names(dt)
  col_names_low <- tolower(col_names)
  # Form regex using OR for searching multiple patterns
  regex <- paste(tolower(to_match), collapse="|")
  # Find
  idx <- grep(pattern=regex, x=col_names_low)
  return(col_names[idx])
}

# ================================================================================================x
# Description     : Returns string w/o leading or trailing whitespace
# Input           : String
# Output          : String
# Review status   : DEV
# ================================================================================================x
# 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# ================================================================================================x
# Description     : Returns classes of data table columns
# Input           : dt
# Output          : res
# Review status   : DEV
# ================================================================================================x
dt_classes <- function(dt) {
  tmp <- t(as.data.table(lapply(dt, class)))
  res <- data.table(col=rownames(tmp), class=tmp[,1])
  return(res)
}

# ================================================================================================x
# Description     : Returns col names of dt belonging to classes specified
# Input           : dt
#                   class_list: e.g. class_list <- c("numeric", "integer")
# Output          : res
# Review status   : DEV
# ================================================================================================x
get_cols <- function(dt, class_list) {
  cl <- dt_classes(dt)
  res <- cl[class %in% class_list]
  return(res)
}

# ================================================================================================x
# Description     : Get most freqent value of vector x
# Input           : x - vector (should be factor)
# Output          : scalar
# Review status   : DEV
# ================================================================================================x
get_most_freq <- function(x, na.rm=F) {
  # Debug: x=sw[store==1001 & CC_year==2013, CC_store_type] 
  if (na.rm){
    x <- x[!is.na(x)]
  }
  return(data.table(x=x)[, .N, by=x][order(N, decreasing = T)][1, x])
}

# ================================================================================================x
# Description     : Compute mean abolute relative prediction error given prediction and target value
# Input           : -
# Output          : -
# Review status   : DEV
# ================================================================================================x
mape <- function(value, prediction, na.rm=T) {
  rel_err <- prediction / value - 1
  return(mean(abs(rel_err), na.rm=na.rm))
}

# ===============================================================================================x
# Description     : Compute slope of var for each store year using weekly data and append to dt_year
# Input           : -
# Output          : dt_year: sy or ety data table
# Review status   : DEV
# ===============================================================================================x
get_year_trend <- function(dt_year, dt_week, var, col_year="CC_year", col_week="week") {
  # Debug: dt_year=ety; dt_week=etw; var="CC_comp_nfs_dist_weighted_sum"; col_year="ev_year"; col_week="ev_week"
  # Add number of data points available for later filtering
  
  sy_n_non_NA <- dt_week[, .(tmp_non_NA=sum(!is.na(get(var)))), by=c("store", col_year)]
  dt_week <- merge(dt_week, sy_n_non_NA, by=c("store", col_year), all.x=T)
  # Compute slope for each year
  trend_year <- dt_week[tmp_non_NA > 20, # 20 to avoid singularity errors
                        .(tmp=lm(as.formula(paste0(var, " ~ ", col_week)), 
                                 data=.SD, 
                                 na.action=na.exclude)$coefficients[2]), # Just take slope
                        by=c("store", col_year)]
  # Rename column
  setnames(trend_year, "tmp", paste0("inyear_trend_", var))
  # Join
  dt_year <- merge(dt_year, trend_year, by=c("store", col_year), all.x=T)
  # Cleanup sw (call by reference)
  dt_week$tmp_non_NA <- NULL
  return(dt_year)
}




##################################################################################################x
# Define ML clases ----
##################################################################################################x


# ================================================================================================x
# R6 class Plot_store_sw ----
# ================================================================================================x
Plot_store_sw <- R6Class(
  classname = "Plot_store_sw",
  
  # ==============================================================================================x
  # Private 
  # ==============================================================================================x
  private = list(
    suffix_interp = "_interp", # Suffix of column base name indicating interpolated data
    suffix_miss = "_miss" # Suffix of column base name indicating missing data
  ), # End private
  
  # ==============================================================================================x
  # Public 
  # ==============================================================================================x
  public = list(
    
    # Attributes
    s=NA,
    cols_2_plot=NA, # Base name of cols to plot
    cols_all=NA, # All columns required from sw
    sw=NA, # Store week data
    class_map=NA, # Data classes of sw cols
    dt_plot=NA,
    
    # --------------------------------------------------------------------------------------------x
    # Constructor
    # Input         : s - Store ID
    #                 sw - Store week table
    #                 cols_2_plot - Vector of column base names to plot
    # Review status : 
    # --------------------------------------------------------------------------------------------x
    initialize = function(s, sw, cols_2_plot) {
      
      self$s <- s
      self$cols_2_plot <- cols_2_plot
      # Get all column required
      tmp <- c(cols_2_plot, paste0(cols_2_plot, private$suffix_miss), paste0(cols_2_plot, private$suffix_interp))
      self$cols_all <- tmp[tmp %in% names(sw)]
      # Save store data
      self$sw <- sw[store==s, c("store", "CC_year", "week", "CC_date", self$cols_all), with=F]
      
      # Remove base columuns if interpolation exists
      for (col in self$cols_2_plot) {
        # Helper col names
        col_interp <- paste0(col, private$suffix_interp)
        # If interpolated column exists
        if (col_interp %in% names(self$sw)) {
          # Remove not-interpolated colum
          self$sw <- self$sw[, names(self$sw)!=col, with=F]
          # Rename interpolated column
          setnames(self$sw, col_interp, col)
        }
      }
      
      # Save class map
      self$class_map <- dt_classes(self$sw[, cols_2_plot, with=F])
    },
    
    # --------------------------------------------------------------------------------------------x
    # Description   : Plot numerical columns
    # Input         : 
    # Review status : DEV
    # --------------------------------------------------------------------------------------------x
    plot_num = function() {
      # Init
      dt_plot_num <- data.table()
      
      # Create plotting data for numeric attributes
      for (col in self$class_map[class=="numeric", col]) {
        # debug: col="CC_nfs_pal"
        
        # Helper col names
        col_miss <- paste0(col, private$suffix_miss)
        
        # If missing helper column exist
        if (col_miss %in% names(self$sw)) {
          tmp <- melt(self$sw[, c("CC_date", col, col_miss), with=F], id.vars=c("CC_date", col_miss))
          setnames(tmp, col_miss, "is_missing")  
          dt_plot_num <- rbind(dt_plot_num, tmp)
        } else {
          # If not, set is_missing col to False by default
          dt_plot_num <- rbind(dt_plot_num, data.table(CC_date=self$sw$CC_date, is_missing=F, variable=col, value=self$sw[, get(col)]))
        }
      }
      # Create plot with numeric values
      p <- ggplot(dt_plot_num) + 
        geom_point(aes(x=CC_date, y=value, color=is_missing)) +
        facet_wrap("variable", nrow=self$class_map[class=="numeric", .N], scales="free") 
      return(p)
    },
    
    
    # --------------------------------------------------------------------------------------------x
    # Description   : Plot factor columns
    # Input         : 
    # Review status : DEV
    # --------------------------------------------------------------------------------------------x
    plot_fac = function() {
      # Create plotting data for factor attributes
      plot_list <- list()
      for (col in self$class_map[class=="factor", col]) {
        # debug: col="shopgroup_db"
        
        # Helper col names
        col_miss <- paste0(col, private$suffix_miss)
        
        # If missing helper column exist
        if (col_miss %in% names(self$sw)) {
          tmp <- melt(self$sw[, c("CC_date", col, col_miss), with=F], id.vars=c("CC_date", col_miss))
          setnames(tmp, col_miss, "is_missing")  
          tmp[, value:=as.factor(value)]
          tmp[, value_num:=as.numeric(value)]
          dt_plot_fac <- tmp
        } else {
          # If not, set is_missing col to False by default
          dt_plot_fac <- data.table(CC_date=self$sw$CC_date, 
                                    is_missing=F, 
                                    variable=col, 
                                    value=factor(self$sw[, get(col)]),
                                    value_num=as.numeric(self$sw[, get(col)])
          )
        }
        
        plot_list <- c(plot_list, list(
          ggplot(dt_plot_fac) +
            geom_point(aes(x=CC_date, y=value_num, color=is_missing, shape=value)) +
            scale_colour_discrete(guide = FALSE) +
            theme(legend.position = c(0.3, 0.2)) +
            ggtitle(unique(dt_plot_fac$variable))
        )
        )
      }
      return(plot_list)
      
    } # end fcn
    
  ) # End public
) # End class

