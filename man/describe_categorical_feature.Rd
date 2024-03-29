% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/help_functions.R
\name{describe_categorical_feature}
\alias{describe_categorical_feature}
\title{Verbal Description of a categorical feature}
\usage{
describe_categorical_feature(
  data,
  clustering,
  cluster_name,
  column_name,
  desc_stop_percentage = 90,
  min_percentage = 5
)
}
\arguments{
\item{data}{the data that was used in order to generate the clusters}

\item{clustering}{the clusters that have been assigned to the data instances}

\item{cluster_name}{the cluster name for which the local scores should be calculated}

\item{column_name}{the name of the column/feature to be described}

\item{desc_stop_percentage}{percentage of combined values after which the description stops}

\item{min_percentage}{minimum percentage a value has to be present in order to be included in the description}
}
\value{
a verbal description of a categorical feature
}
\description{
Verbal Description of a categorical feature
}
\examples{
data <- clusterExplainR::example_mocked_clustering
data_wo_clustering <- data[c(1,2)]
clustering <- data[3]
# generate decription of gender for cluster 1
cat(describe_categorical_feature(data_wo_clustering, clustering, 1, 'Gender'))
cat(describe_categorical_feature(data_wo_clustering, clustering, 1, 'Gender', desc_stop_percentage=75, min_percentage=12))
}
\keyword{internal}
