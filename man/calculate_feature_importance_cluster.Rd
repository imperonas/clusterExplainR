% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_importance.R
\name{calculate_feature_importance_cluster}
\alias{calculate_feature_importance_cluster}
\title{Computae all feature importances for one Cluster}
\usage{
calculate_feature_importance_cluster(
  data_clustered,
  cluster_name,
  numerical_columns,
  categorical_columns,
  cluster_column_name = "Cluster"
)
}
\arguments{
\item{data_clustered}{the clustered dataset}

\item{cluster_name}{the name of the cluster for which the individual feature importances should be generated}

\item{numerical_columns}{vector of indexes of the numerical columns}

\item{categorical_columns}{vector of indexes of the categorical columns}

\item{cluster_column_name}{the name of the column that contains the clusters. Default is 'Cluster'}
}
\value{
a table containing the Cluster_id,Column_name and Importance score for each feature ordered according to the importance score
}
\description{
Calculates the feature importances for all features for a specific cluster.
Feature importance is defined as 1 minus the similarity between the feature entropy within a cluster and the population.
Values are kept between 0 and 1, where 1 indicates a high feature importance, while 0 indicates a no feature importance, since the entropies are equal.
\deqn{FE_{f | c} = 1- ( \frac{H_{f | c}}{H_f})}
}
\examples{
data <- clusterExplainR::example_mocked_clustering
numerical_col_idx <- c(2)
categorical_col_idx <- c(1)
calculate_feature_importance_cluster(data,3,numerical_col_idx,categorical_col_idx)
calculate_feature_importance_cluster(data,1,numerical_col_idx,categorical_col_idx)
calculate_feature_importance_cluster(data,2,numerical_col_idx,categorical_col_idx)
}
\keyword{internal}
