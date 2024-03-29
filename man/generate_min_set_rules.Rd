% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rule_basics.R
\name{generate_min_set_rules}
\alias{generate_min_set_rules}
\title{Generate a minimal set of rules that describes the cluster}
\usage{
generate_min_set_rules(
  data,
  clustering,
  numerical_columns,
  categorical_columns,
  cluster_name
)
}
\arguments{
\item{data}{the data that was used in order to generate the clusters}

\item{clustering}{the clusters that have been assigned to the data instances}

\item{numerical_columns}{all column indexes that are of continuous type}

\item{cluster_name}{the name of the cluster that should be explained}

\item{categoric_columns}{all column indexes that are of categorical/binary/ordinal type}
}
\value{
minimal set of rules:
\item{data}{A dataframe showing the key metrics after a new rule has been added}
\item{rule}{The rule as a conjunction of the rule-set}
\item{verbalization}{Human readable representaion of the ruls}
\item{cluster}{ID of cluster that the rule describes}
}
\description{
Iteratively selects the best feature according to feature importance scores.
For this feature the best rule is selected by optimizing for the F1 Score of possible rules.
}
\examples{
data <- clusterExplainR::example_mocked_clustering
data_wo_clustering <- data[c(1,2)]
clustering <- data[3]
# indicate which columns are categorical and which are numerical
numerical_col_idx <- c(2)
categorical_col_idx <- c(1)
# generate rules that describe cluster 3
rule_3 <- generate_min_set_rules(data[c(1,2)], data[3],numerical_col_idx,categorical_col_idx,3)
cat(rule_3$verbalization)
rule_2 <- generate_min_set_rules(data[c(1,2)], data[3],numerical_col_idx,categorical_col_idx,2)
cat(rule_2$verbalization)
rule_1 <- generate_min_set_rules(data[c(1,2)], data[3],numerical_col_idx,categorical_col_idx,1)
cat(rule_1$verbalization)
}
\keyword{internal}
