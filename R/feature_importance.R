#' Calculates the Shannon Entropy
#'
#'@description
#' Calculates the Shannon Entropy for discrete data.
#' \deqn{H(X) = - \sum_{x \in X} p(x) * \log_2(p(x)) }
#' For continuous data the continuous entropy formula has to be used
#'
#' @param probabilities a list of probabilities, representing the probabilities of distinct feature values
#'
#' @return The Shannon Entropy H
#' @keywords internal
#' @export
#' @examples
#' y = c(.4,.2,.1,.1)
#' compute_discrete_entropy(y)
compute_discrete_entropy <- function(probabilities) {
  # remove 0's from probabilities, since they do not matter in a sum
  probabilities <- probabilities[probabilities != 0]
  entropy <- -sum(probabilities * log2(probabilities))
  return(entropy)
}

#'Cluster Feature Importance for discrete Features
#' @description
#' Calculates the feature-importance for a discrete feature by comparing the entropies of all instances with the instances that fit a specific cluster.
#' Values are kept between 0 and 1, where 1 indicates a high feature importance, while 0 indicates a no feature importance, since the entropies are equal.
#' \deqn{FE_{f | c} = 1- ( \frac{H_{f | c}}{H_f})}
#'
#'
#' @param data_population The dataset including the clustering for all clusters of interest
#' @param column_name the column name of the feature, of whom the feature importance should be assessed
#' @param cluster_column_name the name of the column that represents the clustering
#' @param cluster_name the name of the cluster for which the feature importance should be calculated
#'
#' @return the feature importance as a floating number between 0 and 1, where higher values indicate higher feature importance
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' # Cluster 3 is only composed of Females -> very high Feature Importamce
#' calculate_categoric_featureimportance_discrete_entropy(data, 'Gender', 'Cluster', 3)
#' # Cluster 1 is mainly composed of Females -> feature is important
#' calculate_categoric_featureimportance_discrete_entropy(data, 'Gender', 'Cluster', 1)
#' # Cluster 2 is equally composed of Females and Males -> feature is not important
#' calculate_categoric_featureimportance_discrete_entropy(data, 'Gender', 'Cluster', 2)
calculate_categoric_featureimportance_discrete_entropy <- function(data_population, column_name, cluster_column_name, cluster_name){
  idx = which(colnames(data_population)==column_name)
  # if a feature has only one value, the featureimportance is by default 0, since it has no predictive value
  if (length(unique(data_population[,idx])) == 1){
    return(0)
  }
  cluster_values <- table( data_population[data_population[cluster_column_name] == cluster_name,idx])
  population_values <- table(data_population[,idx])
  prob_cluster_values <- cluster_values / sum(cluster_values)
  prob_population_values <- population_values / sum(population_values)
  pop_entropy <- compute_discrete_entropy(prob_population_values)
  cluster_entropy <- compute_discrete_entropy(prob_cluster_values)
  feature_importance <- 1 - (cluster_entropy/pop_entropy)
  feature_importance <- min(c(1,max(c(0,feature_importance)))) # value has to be geq 0 and leq 1
  return(feature_importance)
}

#'Cluster Feature Importance for continious Features
#' @description
#' Calculates the feature-importance for a continous feature by comparing the continious entropies of all instances with the instances that fit a specific cluster.
#' Values are kept between 0 and 1, where 1 indicates a high feature importance, while 0 indicates a no feature importance, since the entropies are equal.
#' \deqn{FE_{f | c} = 1- ( \frac{h_{f | c}}{h_f})}
#' In order to compare continious entropies over different features, continious entropy is estimated using 100 points within the feature value range
#' and using the NlinTS::entropy_cont function to calculate the continous entropy
#'
#' @param data_population The dataset including the clustering for all clusters of interest
#' @param column_name the column name of the feature, of whom the feature importance should be assessed
#' @param cluster_column_name the name of the column that represents the clustering
#' @param cluster_name the name of the cluster for which the feature importance should be calculated
#'
#' @return the feature importance as a floating number between 0 and 1, where higher values indicate higher feature importance
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' # Cluster 3 contains only individuals with a low weight, while the other clusters show similar distibutions
#' calculate_numeric_featureimportance_continious_entropy(data, 'Weight', 'Cluster', 3)
#' calculate_numeric_featureimportance_continious_entropy(data, 'Weight', 'Cluster', 1)
#' calculate_numeric_featureimportance_continious_entropy(data, 'Weight', 'Cluster', 2)
calculate_numeric_featureimportance_continious_entropy <- function(data_population, column_name, cluster_column_name, cluster_name){
  idx = which(colnames(data_population)==column_name)
  # if a feature has only one value, the featureimportance is by default 0, since it has no predictive value
  if (length(unique(data_population[,idx])) == 1){
    return(0)
  }
  custom_scaling<- function(x, na.rm = FALSE) {
    if(max(x) == min(x)){
      return(0) # only the case if all values are the same
    }
    return((x - max(x)) / (max(x)-min(x)) * 100 +100)
  }
  scaled_data <- data_population %>% mutate_at(c(idx), custom_scaling)
  population <- scaled_data[[column_name]]
  cluster <- scaled_data[scaled_data[[cluster_column_name]] == cluster_name,column_name]
  df_population = approxfun(density(population))
  df_cluster = approxfun(density(cluster))
  # calculate the densities for  selected datapoints (here 100)
  data_test <- data.frame(0:100) %>%
    mutate(pop = df_population(0:100)) %>%
    mutate(cl = df_cluster(0:100)) %>%
    mutate_at(c(2:3), ~replace(., is.na(.), 0))
  pop_entropy <- entropy_cont(data_test$pop, log = "log2")
  cluster_entropy <- entropy_cont(data_test$cl, log = "log2")
  feature_importance <- 1- abs(cluster_entropy/pop_entropy)
  feature_importance <- min(c(1,max(c(0,feature_importance)))) # value has to be geq 0 and leq 1
  return(feature_importance)
}


#' Computae all feature importances for one Cluster
#' @description
#' Calculates the feature importances for all features for a specific cluster.
#' Feature importance is defined as 1 minus the similarity between the feature entropy within a cluster and the population.
#' Values are kept between 0 and 1, where 1 indicates a high feature importance, while 0 indicates a no feature importance, since the entropies are equal.
#' \deqn{FE_{f | c} = 1- ( \frac{H_{f | c}}{H_f})}
#'
#' @param data_clustered the clustered dataset
#' @param cluster_name the name of the cluster for which the individual feature importances should be generated
#' @param numerical_columns vector of indexes of the numerical columns
#' @param categorical_columns vector of indexes of the categorical columns
#' @param cluster_column_name the name of the column that contains the clusters. Default is 'Cluster'
#'
#' @return a table containing the Cluster_id,Column_name and Importance score for each feature ordered according to the importance score
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' numerical_col_idx <- c(2)
#' categorical_col_idx <- c(1)
#' calculate_feature_importance_cluster(data,3,numerical_col_idx,categorical_col_idx)
#' calculate_feature_importance_cluster(data,1,numerical_col_idx,categorical_col_idx)
#' calculate_feature_importance_cluster(data,2,numerical_col_idx,categorical_col_idx)
calculate_feature_importance_cluster <- function(data_clustered, cluster_name, numerical_columns, categorical_columns, cluster_column_name = 'Cluster'){
  # define structure for output table
  feature_importances <- data.table(Cluster_id=character(), Column_name = character(),  Importance_score=numeric())
  for(cat in categorical_columns){
    column_name = colnames(data_clustered)[cat]
    fe = calculate_categoric_featureimportance_discrete_entropy(data_clustered, column_name, cluster_column_name, cluster_name)
    feature_importances <- rbind(feature_importances, list(as.character(cluster_name), column_name, fe))
  }
  for(cont in numerical_columns){
    column_name = colnames(data_clustered)[cont]
    fe = calculate_numeric_featureimportance_continious_entropy(data_clustered, column_name, cluster_column_name, cluster_name)
    feature_importances <- rbind(feature_importances, list(as.character(cluster_name), column_name, fe))
  }
  return(feature_importances%>% arrange(desc(Importance_score)))
}


#' Compute all feature importances for all Cluster
#' @description
#' Calculates the feature importances for all features for each cluster
#' Feature importance is defined as 1 minus the similarity between the feature entropy within a cluster and the population.
#' Values are kept between 0 and 1, where 1 indicates a high feature importance, while 0 indicates a no feature importance, since the entropies are equal.
#' \deqn{FE_{f | c} = 1- ( \frac{H_{f | c}}{H_f})}
#'
#' @param data_clustered the clustered dataset
#' @param numerical_columns vector of indexes of the numerical columns
#' @param categorical_columns vector of indexes of the categorical columns
#' @param cluster_column_name the name of the column that contains the clusters. Default is 'Cluster'
#'
#' @return a table containing the Cluster_id,Column_name and Importance score for each feature ordered according to the importance score
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' numerical_col_idx <- c(2)
#' categorical_col_idx <- c(1)
#' calculate_feature_importance(data,numerical_col_idx,categorical_col_idx)
calculate_feature_importance <- function(data_clustered, numerical_columns, categorical_columns, cluster_column_name = 'Cluster'){
  feature_rankings <- list()
  for(x in unique(data_clustered$Cluster)){ # for loop over individual clusters
    feature_rankings[[x]] <- calculate_feature_importance_cluster(data_clustered, x, numerical_columns, categorical_columns, cluster_column_name)
  }
  return(rbindlist(feature_rankings))
}

