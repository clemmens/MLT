ML <- R6::R6Class(classname="ML",
                  #inherit = ML_data_cleaning,ML_KNN_classifier,
                  lock_objects = FALSE,
                  public=list(
                    get_dt_train_feat = function(){
                      self$dt_train_feat = self$dt[,na.trim(unique(c(self$target,self$train_feat))),with=F]
                    },            
                    # Tree
                    # Auto CP tuning
                    auto_cp_tuning = function(){
                      self$cp = train(
                        as.formula(paste0(self$target, " ~.")),
                        data = self$dt_final_feat,
                        method = "rpart", # Numerical output
                        trControl = trainControl("cv", number = 100), tuneLength=10)
                    },
                    # Auto prune 
                    auto_prune = function(se_factor=1) {
                      cv_perf = as.data.table(self$tree$cptable)
                      # Index corresponding to min xerror
                      idx_min = which.min(cv_perf$xerror)
                      # Optimal CP to avoid overfitting
                      xerror_opt = cv_perf[idx_min, xerror+se_factor*xstd]
                      # Get CP correspoinding to optimal xerror
                      cv_perf[, delta_error:=abs(xerror-xerror_opt)]
                      cp_opt = cv_perf[order(delta_error)][1, CP]
                      # Prune
                      self$tree = prune(self$tree, cp=cp_opt) 
                    },
                    # Build tree model
                    tree_model = function(method="anova"){
                      self$tree = rpart(
                        as.formula(paste0(self$target, " ~.")),
                        data=self$dt_final_feat,
                        method=method, # Numerical output
                        control=rpart.control(
                          minbucket=self$minbucket, # Min number observatiions in leaf 
                          cp=self$cp$bestTune, # Min improvement for split to be acceptable; the lower the more splits are done
                          xval=self$xval, # Number cross validations
                          maxdepth=self$maxdepth # Max allowable tree depth
                        ))
                    },
                    
                    
                    # General
                    shiny_app = NULL,
                    run_date = NULL,
                    filename=NULL,
                    owner = NULL,
                    task = NULL,
                    analysis = NULL,
                    target = NULL,
                    features= NULL,
                    seed=NULL,
                    detail_info=NULL,
                    # General plot
                    result_plot = NULL,
                    # Feature selection
                    feat_sel_bool = NULL,
                    feat_sel_always_keep_cols = NULL,
                    mm = NULL,
                    col_near_zero_var=NULL,
                    col_high_corr= NULL,
                    feat_sel_rem_corr = NULL,
                    feat_sel_rem_corr_cutoff = NULL,
                    feat_sel_rem_low_var = NULL,
                    feat_sel_algorithm = NULL,
                    feat_sel_cutoff = NULL,
                    fit_feature_sel = NULL,
                    var_importance = NULL,
                    dummy_var_cast = NULL,
                    # Balance training
                    balance_set_bool = NULL,
                    balance_method = NULL,
                    # Casting
                    cast_treshold = NULL,
                    # Tree
                    tree = NULL,
                    cp = NULL, # hyperparameter on when to split
                    minbucket = NULL, # minumum size bucket
                    auto_prune_bool = NULL, # Autoprune results using 1SE rule
                    xval = NULL, # Number cross validations
                    maxdepth = NULL, # Maximum dept three
                    # KNN
                    knn = NULL,
                    number_repeatedcv= NULL,
                    repeats_repeatedcv=NULL,
                    preProcess_knn=NULL,
                    metric=NULL,
                    # xgbTree
                    xgbTree = NULL,
                    ## Clustering
                    # Kmeans
                    kmeans = NULL,
                    auto_det_k = NULL,
                    gower_dist = NULL,
                    k_kmeans = NULL,
                    # Clara
                    clara = NULL,
                    clara_standardize = NULL,
                    # data
                    dt = NULL, # uncleaned data
                    dt_train_feat=NULL,
                    dt_final_feat = NULL,
                    dt_result= NULL,
                    train_feat = NULL,
                    final_feat = NULL,
                    # General output
                    dt_clas=NULL,
                    unique_values=NULL,
                    na_overview=NULL,
                    # Initialize function
                    initialize = function(default=T, dt) {
                      self$shiny_app=F
                      self$task = "classification"
                      self$analysis = "tree"
                      self$seed = 123
                      self$features = names(dt)
                      self$detail_info=F
                      # Carat
                      trctrl = NULL
                      # Casting
                      self$cast_treshold = 5
                      # Feature selection
                      self$feat_sel_bool = F
                      self$feat_sel_rem_corr = T
                      self$feat_sel_always_keep_cols = c()
                      self$feat_sel_rem_corr_cutoff = 0.6
                      self$feat_sel_rem_low_var = T
                      self$feat_sel_algorithm = "RF"
                      self$feat_sel_cutoff = 50
                      # Balance training
                      self$balance_set_bool = F
                      self$balance_method = "SMOTE"
                      # Tree
                      self$minbucket = nrow(self$dt)/10 # minumum size bucket
                      self$auto_prune_bool = T # Autoprune results using 1SE rule
                      self$xval = 20 # Number cross validations
                      self$maxdepth = 6 # Maximum dept three
                      # KNN
                      self$number_repeatedcv= 10
                      self$repeats_repeatedcv= 3
                      self$preProcess_knn = c("center", "scale")
                      self$metric = "Accuracy"
                      # Kmeans
                      self$auto_det_k = T
                      self$k_kmeans = 3
                      # Clara
                      self$clara_standardize = T 
                      # data
                      self$dt=dt
                      
                    },
                    
                    # change names to correct names
                    change_names_to_correct_names = function(){
                      names(self$dt_final_feat) = make.names(names(self$dt_final_feat))
                    },
                    
                    # check data quality
                    check_data_quality = function(){
                      if (self$shiny_app==F){
                        print("General statistics")
                        print(self$na_overview)
                        print(self$dt_clas)
                        print("Numeric columns")
                        print(sapply(self$dt_train_feat[,(self$dt_clas[class=="numeric",col]),with=F],function(y) mean(y,na.rm=T)))
                        print(ggplot(stack(self$dt_train_feat[,(self$dt_clas[class=="numeric",col]),with=F]),aes(ind,values))+geom_boxplot())
                        print(ggplot(stack(self$dt_train_feat[,(self$dt_clas[class=="numeric",col]),with=F]),aes(values))  +   facet_wrap(~ ind, scales = "free") + geom_density() )    
                        print("Factor columns")
                        print(self$dt_train_feat[,(self$dt_clas[class=="factor",col]),with=F] %>%
                                # gather to long format
                                gather(na.rm = TRUE) %>%
                                # get the frequency count of key, value columns
                                count(key, value) %>% 
                                ggplot(., aes(x = value, y = n)) + 
                                geom_bar(stat = "identity") + 
                                # facet wrap with key column
                                facet_wrap(~ key)) 
                      }else{
                        self$dt_clas = dt_classes(self$dt)
                        self$unique_values = cbind(as.data.table(apply(self$dt, 2, function(x) length(unique(x)))),colnames(self$dt))
                        names(self$unique_values) <- c("Unique values","Col")
                        self$na_overview = inspect_na(self$dt)
                      }
                    },
                    
                    # get complete cases
                    get_complete_cases = function(){
                      self$dt_final_feat[, cc:=complete.cases(self$dt_final_feat)]
                      self$dt_final_feat[, .N, by=c("cc")]
                      self$dt_final_feat = self$dt_final_feat[cc==T,]
                      self$dt_final_feat$cc = NULL
                    },
                    
                    # casting
                    do_cast = function(){
                      if (length(test$dt_clas[class=="integer",col])!=0)
                      {self$dt_train_feat[, (self$dt_clas[class=="integer",col]) := lapply(.SD, as.numeric), .SDcols=self$dt_clas[class=="integer",col]]}
                      if (length(test$dt_clas[class=="logical",col])!=0)
                      {self$dt_train_feat[, (self$dt_clas[class=="logical",col]) := lapply(.SD, as.factor), .SDcols=self$dt_clas[class=="logical",col]]}
                      if (length(test$dt_clas[class=="Date",col])!=0)
                      {self$dt_train_feat[, (self$dt_clas[class=="Date",col]) := lapply(.SD, as.numeric), .SDcols=self$dt_clas[class=="Date",col]]}
                      to_factor = setdiff(self$unique_values[V1<=self$cast_treshold,V2],self$target)
                      to_numeric = setdiff(self$unique_values[V1>self$cast_treshold,V2],self$target)
                      to_factor = setdiff(self$dt_clas[class=="character",col],to_numeric)
                      to_numeric = setdiff(self$dt_clas[class=="character",col],to_factor)
                      if (length(test$dt_clas[class=="character",col])!=0)
                      {self$dt_train_feat[, (to_factor) := lapply(.SD, as.factor), .SDcols=to_factor]}
                      if (length(test$dt_clas[class=="character",col])!=0)
                      {self$dt_train_feat[, (to_numeric) := lapply(.SD, as.numeric), .SDcols=to_numeric]}
                    },
                    
                    # Get model matrix
                    get_model_matrix = function(){
                      # Update clases
                      feat_factor = self$dt_clas[class=="factor", col]
                      # Model matrix
                      dmg = dummyVars(formula = as.formula(paste0(self$target, "~.")), self$dt_train_feat )
                      # get dummy var relationship table
                      for ( i in 1:length(dmg$facVars)){
                        for ( j in 1:length(dmg$lvls[dmg$facVars[i]][[1]])){
                          self$dummy_var_cast = 
                            rbind(self$dummy_var_cast,as.data.table(t(c(dmg$facVars[i],paste0(dmg$facVars[i],".", dmg$lvls[dmg$facVars[i]][[1]][j])))))
                        }
                      }
                      mm_mat = data.frame(predict(dmg, newdata = self$dt_train_feat))
                      self$mm = cbind(self$dt_train_feat[, as.factor(self$target), with=F], as.data.table(mm_mat))
                    },
                    
                    # Remove near zero variance columns
                    remove_near_zero_var = function(){
                      self$col_near_zero_var = nearZeroVar(self$mm)
                      col = setdiff(self$col_near_zero_var,self$feat_sel_always_keep_cols)
                      if (length(col)!=0){self$mm = self$mm[, -col, with=F]}
                    },
                    
                    # Remove columns with high correlation
                    remove_high_corr = function(){
                      x_cor = cor(self$mm[, -self$target, with=F])
                      idx_corr = findCorrelation(x=x_cor, cutoff = self$feat_sel_rem_corr_cutoff)
                      self$col_high_corr = colnames(x_cor[idx_corr, idx_corr]) 
                      col = setdiff(self$col_high_corr,self$feat_sel_always_keep_cols)
                      if(length(col)!=0){self$mm = self$mm[, -col, with=F]}
                    },
                    
                    # Calculate gower distance
                    calc_gower_dist = function(){
                      self$gower_dist <- cluster::daisy(self$dt_final_feat, metric="gower")
                    },
                    
                    # Determine number of kmeans clusters
                    cal_nb_k_clusters = function(){
                      # Calculate silhouette width for many k using PAM
                      sil_width <- c(NA)
                      for(i in 2:10){
                        pam_fit <- cluster::pam(self$gower_dist,
                                                diss = TRUE,
                                                k = i)
                        sil_width[i] <- pam_fit$silinfo$avg.width
                      }
                      if (self$detail_info==T){
                        plot(1:10, sil_width,
                             xlab = "Number of clusters",
                             ylab = "Silhouette Width")
                        lines(1:10, sil_width)
                      }
                      nb = NbClust(as.matrix(self$gower_dist),distance  = "euclidean",index=c("silhouette","ch","ptbiserial"),min.nc = 2,max.nc = 10,method="kmeans")
                      self$k_kmeans <- get_most_freq(c(as.numeric(nb$Best.nc[1]),as.numeric(nb$Best.nc[3]),as.numeric(nb$Best.nc[5])))
                    },
                    
                    # Get final dataset
                    get_dt_final_feat = function(){
                      if (is.null(self$target)==F){
                        train_all = self$dt_train_feat[is.na(get(self$target))==F ,]
                        self$dt_final_feat = train_all[, na.trim(unique(c(self$target, self$final_feat))), with=F]
                      }else{
                        train_all = self$dt_train_feat[ ,]
                        self$dt_final_feat = train_all[, na.trim(unique(c(self$target, self$final_feat))), with=F]
                      }
                    },
                    
                    # Feature selection RF
                    feature_sel_rf = function(){
                      #browser()
                      names(self$mm) = make.names(names(self$mm))
                      x = self$mm[, -self$target, with=F]
                      y = self$mm[, get(self$target)]
                      #train control
                      ctrl = trainControl(method="repeatedcv",
                                          repeats=1,
                                          number = 5, # number of folds
                                          sampling="smote",
                                          #classProbs=T,
                                          selectionFunction = "oneSE"
                      )
                      grid = expand.grid( .winnow = c(TRUE,FALSE), .trials=c(10,20), .model="tree" )
                      # Model
                      self$fit_feature_sel = train(x=x,
                                                   y=y,
                                                   method = "C5.0",
                                                   trControl = ctrl,
                                                   metric = "Accuracy",
                                                   importance=T,
                                                   verbose = F,
                                                   tuneGrid = grid) # set from grid to NULL to swich off manual tuning grid
                      self$var_importance = varImp(self$fit_feature_sel)
                    },
                    
                    # Tree main
                    main_tree = function(){
                      self$get_complete_cases()
                      self$change_names_to_correct_names()
                      self$dt_final_feat[, (self$target) := lapply(.SD, as.factor), .SDcols=self$target]
                      self$auto_cp_tuning()
                      self$tree_model()
                      if (self$auto_prune_bool==T){self$auto_prune()}
                      self$dt_result = add_cluster_col(self$dt, self$tree)
                    },
                    
                    # Feature selection
                    feature_selection = function(){
                      if(self$task=="classification"){
                        self$get_model_matrix()
                        # Remove cor and low var features
                        if(self$feat_sel_rem_low_var==T){self$remove_near_zero_var()}
                        if(self$feat_sel_rem_corr==T){self$remove_high_corr()}
                        # feature selection
                        self$feature_sel_rf()
                        # get final features
                        col_index = self$var_importance$importance %>% mutate(names=row.names(.)) %>% arrange(-Overall) %>% as.data.table()
                        col_index <- col_index[Overall>=self$feat_sel_cutoff,names]
                        for (i in 1:length(col_index)){
                          if(length(self$dummy_var_cast[V2==col_index[i],V1])!=0){col_index[i]=self$dummy_var_cast[V2==col_index[i],V1]}
                        }
                        col_index = unique(col_index)
                        self$final_feat = col_index
                      }else{self$final_feat = self$train_feat}
                    },  
                    
                    set_traincontrol = function(){
                      self$trctrl = trainControl(method = "repeatedcv", number = self$number_repeatedcv, repeats = self$repeats_repeatedcv)
                    },
                    
                    # KNN classifier
                    main_knn = function(){
                      self$set_traincontrol()
                      self$knn = train( as.formula(paste0(self$target, " ~.")), data = self$dt_final_feat, method = "knn",
                                        trControl =self$trctrl,
                                        preProcess = self$preProcess_knn,
                                        tuneLength = 10#,
                                        #metric = self$metric
                      )
                      self$dt_result <- cbind(self$dt,as.data.table(self$knn %>% predict(self$dt)))
                      
                    },
                    
                    # XGBOOS classifier
                    main_xgboosttree = function(){
                      self$set_traincontrol()
                      self$xgbTree = train( as.formula(paste0(self$target, " ~.")), data = self$dt_final_feat, method = "xgbTree",
                                            trControl =self$trctrl,
                                            tuneLength = 1
                      )
                      self$dt_result <- cbind(self$dt,as.data.table(self$xgbTree %>% predict(self$dt_final_feat)))
                    },
                    
                    # Clara clustering
                    main_clara = function(){
                      self$calc_gower_dist()
                      if (self$auto_det_k){
                        self$cal_nb_k_clusters()
                      }
                      self$clara <- cluster::clara(self$dt_final_feat, self$k_kmeans,metric = "euclidean", samples = 50, pamLike = TRUE,stand=self$clara_standardize)
                      if (self$detail_info==T){
                        # Print components of clara
                        print(self$clara)
                      }
                      # Add column with results
                      self$dt_result <- cbind(self$dt, cluster = self$clara$cluster)
                    },
                    
                    # Kmeans clustering
                    main_kmeans = function(){
                      self$calc_gower_dist()
                      if (self$auto_det_k){
                        self$cal_nb_k_clusters()
                      }
                      self$kmeans <- kmeans(self$gower_dist, self$k_kmeans, nstart = 25)
                      if (self$detail_info==T){
                        browser()
                        print(self$kmeans)
                      }
                      self$dt_result <- cbind(self$dt,cluster=self$kmeans$cluster)
                    },
                    
                    # Display results
                    results = function(){
                      if(self$feat_sel_bool==T){
                        print("variable importance")
                        print(self$var_importance)
                        print("final features")
                        print(self$final_feat)}
                      if(self$analysis=="tree"){ 
                        if (self$shiny_app==T){
                          return(rpart.plot(self$tree, digits=3))
                        }else{
                          (rpart.plot(self$tree, digits=3))
                          if (self$detail_info==T){
                            print(plotcp(self$tree))
                          } 
                        }
                      }
                      if (self$analysis=="knn"){
                        print("best tune")
                        print(self$knn$bestTune)
                        split_data <- PCAmixdata::splitmix(self$dt_final_feat)
                        obj <- PCAmixdata::PCAmix(X.quanti=split_data$X.quanti,X.quali=split_data$X.quali,ndim=2)
                        if (self$shiny_app==T){
                          return(print(plot(obj,choice="ind",coloring.ind=as.factor(predict(test$knn)),label=FALSE,
                                            posleg=NA, main="Observations")))
                          #return(ggplot(data = self$dt_final,aes(self$final_feat[1],self$final_feat[2],color="V1"))+geom_point())
                        }else{
                            print(plot(obj,choice="ind",coloring.ind=as.factor(predict(test$knn)),label=FALSE,
                                       posleg=NA, main="Observations"))
                          if (self$detail_info==T){
                            print("KNN performance")
                            print(plot(self$knn))
                          } 
                          }
                          
                          #ggplot(data = self$dt_final,aes(self$final_feat[1],self$final_feat[2],color="V1"))+geom_point()
                      }
                      if (self$analysis=="xgbTree"){
                        split_data <- PCAmixdata::splitmix(self$dt_final_feat)
                        obj <- PCAmixdata::PCAmix(X.quanti=split_data$X.quanti,X.quali=split_data$X.quali,ndim=2)
                        if (self$shiny_app==T){
                          return(print(plot(obj,choice="ind",coloring.ind=as.factor(predict(test$xgbTree)),label=FALSE,
                                            posleg=NA, main="Observations")))
                        }else{
                          print(plot(obj,choice="ind",coloring.ind=as.factor(predict(test$xgbTree)),label=FALSE,
                                     posleg=NA, main="Observations"))
                        }
                        #print(plot(self$xgbTree))
                      }
                      if(self$analysis=="kmeans"){
                        split_data <- PCAmixdata::splitmix(self$dt_final_feat)
                        obj <- PCAmixdata::PCAmix(X.quanti=split_data$X.quanti,X.quali=split_data$X.quali,ndim=2)
                        if (self$shiny_app==T){
                          return(print(plot(obj,choice="ind",coloring.ind=as.factor(self$kmeans$cluster),label=FALSE,
                                            posleg=NA, main="Observations")))
                        }else{
                          print(plot(obj,choice="ind",coloring.ind=as.factor(self$kmeans$cluster),label=FALSE,
                                     posleg=NA, main="Observations"))
                          print(plot(obj,choice="ind",coloring.ind=as.factor(self$kmeans$cluster),label=FALSE,
                                     posleg=NA, main="Observations"))
                        }
                        
                      }
                      if(self$analysis=="clara"){
                        if(self$shiny_app==T){
                          return(
                            fviz_cluster(self$clara, 
                                         #palette = c("#00AFBB", "#FC4E07"), # color palette
                                         ellipse.type = "t", # Concentration ellipse
                                         geom = "point", pointsize = 1,
                                         ggtheme = theme_classic()
                            )
                          )
                        }else{
                          print(fviz_cluster(self$clara, 
                                             #palette = c("#00AFBB", "#FC4E07"), # color palette
                                             ellipse.type = "t", # Concentration ellipse
                                             geom = "point", pointsize = 1,
                                             ggtheme = theme_classic()
                          )) 
                        }
                      }
                    },
                    result_metric = function(){
                      return()
                    },
                    
                    # Main
                    main_ML = function(){
                      set.seed=self$seed
                      if(is.null(self$train_feat)==T){self$train_feat=setdiff(names(self$dt),self$target)}
                      # Select user required features and target
                      self$get_dt_train_feat()
                      # Get key insights
                      self$dt_clas = dt_classes(self$dt_train_feat)
                      self$unique_values = cbind(as.data.table(apply(self$dt_train_feat, 2, function(x) length(unique(x)))),colnames(self$dt_train_feat))
                      self$na_overview = inspect_na(self$dt_train_feat)
                      # Remove NA from dt_train_feat
                      self$dt_train_feat = remove_inf(self$dt_train_feat)
                      # Casting
                      self$do_cast()
                      # Check data quality
                      if (self$detail_info==T){self$check_data_quality()}
                      # Main
                      if(self$task!="feature importance"){
                        if(self$feat_sel_bool==T & self$task!=c("clustering")){
                          self$feature_selection()
                          self$get_dt_final_feat()
                        }else{
                          self$final_feat = self$train_feat
                          self$get_dt_final_feat()
                        }
                        
                        if (self$task=="classification"){
                          if (self$analysis=="tree"){
                            self$main_tree()
                          }
                          if (self$analysis=="knn"){
                            self$main_knn()
                          }
                          if (self$analysis=="xgbTree"){
                            self$main_xgboosttree()
                          } 
                          if(self$analysis %ni% c("tree","knn","xgbTree")){
                            print(paste("Analysis",self$analysis,"not supported"))
                          }
                        }
                        if (self$task=="clustering"){
                          if(self$analysis=="kmeans"){
                            self$main_kmeans()
                          } 
                          if  (self$analysis=="clara"){
                            self$main_clara()
                          }
                          if(self$analysis %ni% c("kmeans","clara")){
                            print(paste("Analysis",self$analysis,"not supported"))
                          }
                        }
                        if (self$task=="regression"){
                          if(self$analysis==""){}
                        }
                        if(self$task %ni% c("clustering","regression","classification")){
                          print(paste("Task",self$task,"not supported"))
                        }
                        
                      }
                      # Else do feature importance
                      #else{
                      
                      #}# end feature importance
                      
                    }
                    
                  ))