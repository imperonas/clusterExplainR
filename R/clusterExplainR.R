#' clusterExplainR
#'
#' The basic idea is the creation of explanations for mixed data clusters
#' purely from feature value distributions within and between clusters.
#' These explanations consist of:
#'
#'  1. Global Explanations: Feature Importance Scores and Feature Visualizations
#'
#'  2. Local Explanations: Feature Importance Scores, Visualizations, Cluster Representatives and Rules
#'
#' @docType package
#' @name clusterExplainR
NULL

#' Local Cluster Explanations
#'
#'@description
#' Explains a cluster by ordering the features according to their importance.
#' The local feature importance scores show which features are important for
#' a given cluster, and are calculated by comparing the entropy for a feature
#' over all clusters vs within a specific cluster.
#' If the entropy within a cluster is lower than within the population
#' (all clusters combined) the feature importance is high.
#' For categorical features the classical discrete entropy is used,
#'  while for continuous features the discrete entropy is estimated.
#' Based on local feature importance, the n-most important features can be
#' visualized. Additionally, based on the importance scores entities can be
#' assigned an EntityClusterMatchingScore. Therefore, for each entity it is
#' compared how well its feature value fits within the cluster distribution and
#' is further weighted by the feature importance score. Ergo, a cluster
#' representative should be in the center of the cluster within those
#' features that are more important.Moreover a conjunction rule set is created
#' that describes the feature. Rules are created to optimize the F1 Score in
#' order to provide Accuracy and Coverage.
#'
#' @param data the data that was used in order to generate the clusters
#' @param clusterings the clusters that have been assigned to the data instances
#' @param numerical_columns all columns indexes that are of numerical/continuous type
#' @param categorical_columns all column indexes that are of categorical/binary/ordinal type
#' @param cluster the cluster name for which the local scores should be calculated
#' @param generate_rules boolean to indicate if rules for individual clusters should be generated
#' @param generate_prototypes boolean to indicate if prototypes via ECMS for individual clusters should be generated
#'
#' @return An explanation of a cluster based on feature importance that compare the cluster with a population:
#'  \item{feature_importance}{returns a table that contains Cluster_id (name of the cluster), Column_name (name of the column -- the feature), Importance_score (the actual feature importance). Wraps: \link[clusterExplainR]{calculate_feature_importance_cluster}}
#'  \item{inspect_feature(column_name)}{function that can be called with a column name and returns a verbalization, the feature importance as well as a visualization (plot)}
#'  \item{top_n_features(number_of_features)}{function that returns a visualization of the n most important features. For each feature a subplot is created that shows the feature values the cluster takes in combination with the population values}
#'  \item{ECMS}{a vector that contains one entry per data row representing th EntityClusterMatchingScore (higher values suggest good fit into a cluster) \link[clusterExplainR]{calculate_ECMS}}
#'  \item{cluster_represenatative(number_of_entities)}{returns a list of cluster representatives, the n representatives with the highest ECMS that were assigned the cluster.}
#'  \item{rule}{if rules are generated, contains a descriptive rule of the cluster. For details, see \link[clusterExplainR]{generate_min_set_rules}}
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' data_wo_clustering <- data[c(1,2)]
#' clustering <- data[[3]]
#' # indicate which columns are categorical and which are numerical
#' numerical_col_idx <- c(2)
#' categorical_col_idx <- c(1)
#' # explain cluster 3
#' explanation_3 <- explain_cluster(data_wo_clustering, clustering,numerical_col_idx,categorical_col_idx,3)
#' head(explanation_3$feature_importance,2)
#' explanation_3$inspect_feature('Weight')$plot
#' explanation_3$top_n_features(2)
#' explanation_3$cluster_representative(2)
#' cat(explanation_3$rule$verbalization)
explain_cluster <- function(data, clusterings, numerical_columns, categorical_columns, cluster, generate_rules = TRUE, generate_prototypes = TRUE){
  data_clustered <- data %>% mutate(Cluster=clusterings)
  ret <- list()
  ret$feature_importance <- calculate_feature_importance_cluster(data_clustered, cluster, numerical_columns, categorical_columns)
  ret$inspect_feature <- function(column_name){
      verbalisation <- describe_feature(data, clusterings, categorical_columns, cluster, column_name)
      fe_imp <- ret$feature_importance %>% filter(Column_name == column_name)
      p <-  display_colum_cluster_vs_population(data %>% mutate(Cluster=clusterings),categorical_columns, cluster, column_name)+
        labs(subtitle = paste(column_name,'\n   ', round(fe_imp[1,3], digits=2)) , title = NULL, x = NULL, y = NULL) +
        theme(legend.position="none")
      p <- annotate_figure(p,bottom = textGrob(verbalisation, gp = gpar(cex =.9)))
      return(list(verbalisation = verbalisation, feature_importance = fe_imp, plot = p))
  }
  ret$top_n_features <- function(number_of_features){
    p <- plot_top_n_most_important_features(data, clusterings, numerical_columns, categorical_columns, cluster, ret$feature_importance, number_of_features)
      return(p)
  }
  if(generate_prototypes){
    ret$ECMS <- vector()
    for(instance_position in 1:nrow(data_clustered)){
      instance_of_interest <- data[instance_position, ]
      ecms_temp <- calculate_ECMS(data, clusterings, ret$feature_importance, categorical_columns, cluster, instance_of_interest)
      ret$ECMS[length(ret$ECMS)+1] <- ecms_temp$ECMS_total
    }
    ret$cluster_representative <- function(number_of_entities){
      data_clustered_with_ecms <- data_clustered %>% mutate(ECMS = ret$ECMS)
      head(data_clustered_with_ecms %>% filter(Cluster == cluster)  %>% arrange(desc(ECMS)), number_of_entities)
    }
  }
  if(generate_rules){
    ret$rule <- generate_min_set_rules(data, clusterings, numerical_columns, categorical_columns, cluster)
  }
  return(ret)
}

#' Global Explanations
#'
#' @description
#' Calculates the global feature importance scores as the mean feature
#' importance for each feature and individual cluster. This should provide
#' the user with an overall overview which features are the overall most
#' important features that separate the clusters. It is build on the
#' function \link[clusterExplainR]{calculate_local_feature_importance}.
#'
#'
#' @param data the data that was used in order to generate the clusters
#' @param clusterings the clusters that have been assigned to the data instances
#' @param numerical_columns all columns indexes that are of numerical/continuous type
#' @param categorical_columns all column indexes that are of categorical/binary/ordinal type
#'
#' @return An explanation of a clustering based on feature importance for individual clusters:
#'  \item{feature_importance}{returns a table that contains Column_name, mean_Importance_score, min_Importance_score and max_Importance_score for each feature. Aggregations max,mean,min are done over all clusters without weighting by cluster sizes. For individual feature importance calculations see: \link[clusterExplainR]{calculate_feature_importance_cluster}.}
#'  \item{top_n_features(number_of_features, type = 'mean')}{function that returns a visualization of the n most important features overall (Moreover, the type refers to the aggregation over clusters and can be 'mean','max' or 'min'). For each feature a subplot is created that shows the feature distributions among the individual clusters.}
#'  \item{fis_plot}{plots the 10 most important feature importance scores as a barplot.}
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' data_wo_clustering <- data[c(1,2)]
#' clustering <- data[[3]]
#' # indicate which columns are categorical and which are numerical
#' numerical_col_idx <- c(2)
#' categorical_col_idx <- c(1)
#' # calculate the globale feature importance
#' exp <- explain_global_clustering(data_wo_clustering, clustering,numerical_col_idx,categorical_col_idx)
#' exp$feature_importance
#' exp$top_n_features(2)
#' exp$fis_plot
explain_global_clustering <- function(data, clusterings, numerical_columns, categorical_columns){
  data_clustered <- data %>% mutate(Cluster=clusterings)
  # TODO should this also include explanations for each cluster??
  fe_per_cluster <- calculate_feature_importance(data_clustered, numerical_columns, categorical_columns)
  ret <- list()
  ret$feature_importance <- fe_per_cluster %>%
    group_by(Column_name) %>%
    select(Column_name, Importance_score) %>%
    summarise(mean_Importance_score = mean(Importance_score), min_Importance_score = min(Importance_score), max_Importance_score = max(Importance_score)) %>%
    arrange(desc(mean_Importance_score))
  ret$top_n_features <- function(number_of_features, type = 'mean'){
    p <-display_top_n_global_features(data_clustered, categorical_columns, ret$feature_importance,number_of_features, type)
    return(p)
  }
  top_t_features <- ret$feature_importance %>% .[['Column_name']]
  ret$fis_plot <- ggplot(data= fe_per_cluster %>% mutate(Column_name = factor(Column_name, levels= rev(top_t_features))) %>% mutate(Importance_score = Importance_score/3) %>%filter(Column_name %in% top_t_features), aes(x=Column_name, y=Importance_score, fill=Cluster_id)) +
    geom_bar(stat="identity") +
    coord_flip()+
    ylab('GFIS') +xlab('Feature')

  return(ret)
}

