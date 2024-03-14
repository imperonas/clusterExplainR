#' Matching Score for categorical/descrete variables
#'
#' @description
#' The Matching Score evaluates how well a value matches the cluster values.
#' Basic idea, comparing the probability of a given value for which matching score should be calculated, with highest probability of a single value.
#' \deqn{MS_{x|f_c} = P(x | f_c)/ max_{v \in f_c}(P(v | f_c))}
#'  Takes a value range [0,1]
#'
#' @param data_clustered the clustered Dataset, expects the column with the clustering to be named 'Cluster'
#' @param column_name the feature/column for which the matching score should be calculates
#' @param cluster_name the name of the cluster for which the individual matching score should be generated
#' @param value a value for the desired feature
#'
#' @return a matching score of a value for a given feature/column
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' # In Cluster 1 there are 7 females and 1 male
#' # for 'Female': MS = (7/8)/(7/8) = 1 (since Female is most probable value)
#' categoric_matching_score(data,'Gender',1,'Female')
#' # for 'Male': MS = (1/8)/(7/8) = 0.1428571 (since Male is a less probable value)
#' categoric_matching_score(data,'Gender',1,'Male')
categoric_matching_score <- function(data_clustered, column_name, cluster_name, value){
  idx = which(colnames(data_clustered)==column_name)
  cluster_values <- table( data_clustered[data_clustered$Cluster == cluster_name,idx])
  prob_cluster_values <- cluster_values / sum(cluster_values)
  value_prob = prob_cluster_values[value]
  max_prob = max(prob_cluster_values)
  matching = value_prob/max_prob
  if (is.na(matching)){
    return(0)
  }
  return(matching)
}

#' Matching Score for numerical/continious variables
#'
#' @description
#' The Matching Score evaluates how well a value matches the cluster values.
#' Basic idea, comparing the probability of a given value for which matching score should be calculated, with highest probability of a single value.
#' For continious data the probabilities are estimated by approximating the density distributions.
#' \deqn{MS_{x|f_c} = d(x)/ max_{v \in f_c}(d(v))}
#'  Takes a value range [0,1]
#'
#' @param data_clustered the clustered Dataset, expects the column with the clustering to be named 'Cluster'
#' @param column_name the feature/column for which the matching score should be calculates
#' @param cluster_name the name of the cluster for which the individual matching score should be generated
#' @param value a value for the desired feature
#'
#' @return a matching score of a value for a given feature/column
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' numeric_matching_score(data,'Weight',2,50)
#' numeric_matching_score(data,'Weight',2,68)
#' numeric_matching_score(data,'Weight',2,90)
numeric_matching_score = function(data_clustered, column_name, cluster_name, value){
  idx = which(colnames(data_clustered)==column_name)
  cl_data = data_clustered %>% filter(Cluster == cluster_name)
  cl_values <- cl_data[,idx]
  cl_density <- density(cl_values)
  max_prob = max(cl_density$y)
  density_function = approxfun(cl_density)
  value_prob = density_function(value)
  matching = value_prob/max_prob
  if (is.na(matching)){
    return(0)
  }
  return(matching)
}


#' Calculate Entity Cluster Matching Score
#'
#'@description
#' Calculates the Entity Cluster Matching Scores (ECMS) which indicate how well an instance fits a specific cluster.
#' An instance with a high ECMS fits the cluster better than an instance with a lower ECMS.
#' \deqn{ECMS(x,c) = \frac{ \sum_{f \in F} p(x_f,c_f) * FIS(f,c)}{ \sum_{f \in F} FIS(f,c)}}
#'
#' @param data the data that was used in order to generate the clusters
#' @param clusterings the clusters that have been assigned to the data instances
#' @param feature_importance the importance of features as calculated by \link[clusterExplainR]{calculate_feature_importance_cluster}
#' @param categorical_columns all column indexes that are of categorical/binary/ordinal type
#' @param cluster_name the name of the cluster for which ECMS should be calculated
#' @param instance an instance from the dataset, use one row from table of all instances
#'
#' @return The Entity cluster matching score as well as individual scores per feature:
#'  \item{ECMS_total}{the calculated ECMS}
#'  \item{EMS}{a table that showns the indicidual matching scores per feature as well as feature importances}
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' data_wo_clustering <- data[c(1,2)]
#' clustering <- data[[3]]
#' # indicate which columns are categorical and which are numerical
#' numerical_col_idx <- c(2)
#' categorical_col_idx <- c(1)
#' # calculate feature importance scores
#' cluster_of_interest <- 1
#' instance_of_interest <- data_wo_clustering[1,]
#' fis <- calculate_feature_importance_cluster(data,cluster_of_interest,numerical_col_idx,categorical_col_idx)
#' ecms <- calculate_ECMS(data_wo_clustering, clustering, fis, categorical_col_idx, cluster_of_interest, instance_of_interest)
#' # the overall matching score of the instance to the cluster
#' ecms$ECMS_total
#' # shows the individual matching scores per feature
#' ecms$EMS
calculate_ECMS <- function(data, clustering, feature_importance, categorical_columns, cluster_name, instance){
  data <- data %>% mutate(Cluster = clustering)
  df = data.frame(
    Column_name=character(),
    ECMS=double(),
    ms=double(),
    fi=double(),
    stringsAsFactors=FALSE)
  for (column_id in 1:(ncol(data) -1)) {
    column_name = colnames(data)[column_id]
    if (column_id %in% categorical_columns ) {
      ms = categoric_matching_score(data, column_name, cluster_name, instance[1,column_id])
    }
    else{
      ms = numeric_matching_score(data, column_name, cluster_name, instance[1,column_id])
    }
    fis = feature_importance %>%
      filter(Cluster_id == cluster_name) %>%
      filter(Column_name == column_name) %>%
      select(Importance_score)
    fi = fis[[1,1]]
    ecms = ms * fi
    df <- df %>% add_row( Column_name= column_name, ECMS = ecms, ms = ms, fi = fi)
  }
  colnames(df)<- c('Column_name', 'ECMS', 'MatchingScore', 'FeatureImportance')
  return(list(ECMS_total = sum(df$ECMS)/sum(df$FeatureImportance), EMS=df))
}
