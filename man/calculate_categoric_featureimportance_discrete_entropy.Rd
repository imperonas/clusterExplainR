% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_importance.R
\name{calculate_categoric_featureimportance_discrete_entropy}
\alias{calculate_categoric_featureimportance_discrete_entropy}
\title{Cluster Feature Importance for discrete Features}
\usage{
calculate_categoric_featureimportance_discrete_entropy(
  data_population,
  column_name,
  cluster_column_name,
  cluster_name
)
}
\arguments{
\item{data_population}{The dataset including the clustering for all clusters of interest}

\item{column_name}{the column name of the feature, of whom the feature importance should be assessed}

\item{cluster_column_name}{the name of the column that represents the clustering}

\item{cluster_name}{the name of the cluster for which the feature importance should be calculated}
}
\value{
the feature importance as a floating number between 0 and 1, where higher values indicate higher feature importance
}
\description{
Calculates the feature-importance for a discrete feature by comparing the entropies of all instances with the instances that fit a specific cluster.
Values are kept between 0 and 1, where 1 indicates a high feature importance, while 0 indicates a no feature importance, since the entropies are equal.
\deqn{FE_{f | c} = 1- ( \frac{H_{f | c}}{H_f})}
}
\examples{
data <- clusterExplainR::example_mocked_clustering
# Cluster 3 is only composed of Females -> very high Feature Importamce
calculate_categoric_featureimportance_discrete_entropy(data, 'Gender', 'Cluster', 3)
# Cluster 1 is mainly composed of Females -> feature is important
calculate_categoric_featureimportance_discrete_entropy(data, 'Gender', 'Cluster', 1)
# Cluster 2 is equally composed of Females and Males -> feature is not important
calculate_categoric_featureimportance_discrete_entropy(data, 'Gender', 'Cluster', 2)
}
\keyword{internal}
