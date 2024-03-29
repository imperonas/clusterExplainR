% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusterExplainR.R
\name{explain_global_clustering}
\alias{explain_global_clustering}
\title{Global Explanations}
\usage{
explain_global_clustering(
  data,
  clusterings,
  numerical_columns,
  categorical_columns
)
}
\arguments{
\item{data}{the data that was used in order to generate the clusters}

\item{clusterings}{the clusters that have been assigned to the data instances}

\item{numerical_columns}{all columns indexes that are of numerical/continuous type}

\item{categorical_columns}{all column indexes that are of categorical/binary/ordinal type}
}
\value{
An explanation of a clustering based on feature importance for individual clusters:
 \item{feature_importance}{returns a table that contains Column_name, mean_Importance_score, min_Importance_score and max_Importance_score for each feature. Aggregations max,mean,min are done over all clusters without weighting by cluster sizes. For individual feature importance calculations see: \link[clusterExplainR]{calculate_feature_importance_cluster}.}
 \item{top_n_features(number_of_features, type = 'mean')}{function that returns a visualization of the n most important features overall (Moreover, the type refers to the aggregation over clusters and can be 'mean','max' or 'min'). For each feature a subplot is created that shows the feature distributions among the individual clusters.}
 \item{fis_plot}{plots the 10 most important feature importance scores as a barplot.}
}
\description{
Calculates the global feature importance scores as the mean feature
importance for each feature and individual cluster. This should provide
the user with an overall overview which features are the overall most
important features that separate the clusters. It is build on the
function \link[clusterExplainR]{calculate_local_feature_importance}.
}
\examples{
data <- clusterExplainR::example_mocked_clustering
data_wo_clustering <- data[c(1,2)]
clustering <- data[[3]]
# indicate which columns are categorical and which are numerical
numerical_col_idx <- c(2)
categorical_col_idx <- c(1)
# calculate the globale feature importance
exp <- explain_global_clustering(data_wo_clustering, clustering,numerical_col_idx,categorical_col_idx)
exp$feature_importance
exp$top_n_features(2)
exp$fis_plot
}
