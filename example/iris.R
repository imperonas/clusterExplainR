# load the iris dataset
data(iris)
head(iris,2)

library(clusterExplainR)

global_iris_exp <- clusterExplainR::explain_global_clustering(
  data=iris[c(1,2,3,4)],
  clusterings = iris[[5]],
  numerical_columns = 1:4,
  categorical_columns = c()
)
global_iris_exp$feature_importance
global_iris_exp$top_n_features(4)




iris_local_explanations <- list()
interesting_clusters <- unique(iris[[5]])
for (c in interesting_clusters) {
  iris_local_explanations[[c]] <- clusterExplainR::explain_cluster(
    data=iris[c(1,2,3,4)],
    clusterings = iris[[5]],
    numerical_columns = 1:4,
    categorical_columns = c(),
    cluster = c,
    generate_rules = TRUE
  )
}

iris_local_explanations[['setosa']]$feature_importance

# describe clusters by rules
for (c  in interesting_clusters) {
  cat(paste(iris_local_explanations[[c]]$rule$verbalization,'\n\n'))
}

iris_local_explanations[['setosa']]$rule$data

