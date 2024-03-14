# convenient definition of the inverse of %in% function, using same syntax as dplyr
'%!in%' <- function(x,y){
  !('%in%'(x,y))
}


#' Example Data generation
#'
#' @return an example dataset
#' @keywords internal
#' @export
#' @examples
#' dataset <- generate_example_dataset()
#'
generate_example_dataset <- function(){
  set.seed(42)
  mutate_gender <- function(x){
    if(x>12){
      return('Male')
    }
    return('Female')
  }
  simple_dataset <- data.frame(
    Gender = sapply(1:20,mutate_gender), # assign gender 60% female 40% male
    Weight = round(rnorm(20, mean=70, sd=10), 1), # select weights from normal distributions
    Cluster=factor(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,1)) # assign clusters, without actual clustering
  )
  return(simple_dataset)
}


#########################
# verbalisation functions
#########################

#' Verbal Description of a categorical feature
#'
#' @param data the data that was used in order to generate the clusters
#' @param clustering the clusters that have been assigned to the data instances
#' @param cluster_name the cluster name for which the local scores should be calculated
#' @param column_name the name of the column/feature to be described
#' @param desc_stop_percentage percentage of combined values after which the description stops
#' @param min_percentage minimum percentage a value has to be present in order to be included in the description
#'
#' @return a verbal description of a categorical feature
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' data_wo_clustering <- data[c(1,2)]
#' clustering <- data[3]
#' # generate decription of gender for cluster 1
#' cat(describe_categorical_feature(data_wo_clustering, clustering, 1, 'Gender'))
#' cat(describe_categorical_feature(data_wo_clustering, clustering, 1, 'Gender', desc_stop_percentage=75, min_percentage=12))
describe_categorical_feature <- function(data, clustering, cluster_name, column_name, desc_stop_percentage=90, min_percentage=5){
  data_clustered <-  data %>% mutate(Cluster = clustering)
  x <- as.data.frame(data_clustered %>% filter(Cluster == cluster_name) %>% select(column_name) %>% table())
  x <- x %>% mutate(Percentage = Freq/sum(Freq) * 100) %>% arrange(desc(Percentage))
  actual <- 0
  desc <- paste("Cluster",cluster_name,"is described by", column_name)
  for (i in 1:nrow(x)) {
    if(actual >= desc_stop_percentage ){
      desc <- paste(desc, "\n  - ", round(100-actual, digits = 0), '% other')
      break
    }
    if(x[i, "Percentage"] <= min_percentage){
      desc <- paste(desc, "\n  - ", round(100-actual, digits = 0), '% other')
      break
    }
    desc <- paste(desc,"\n  - ",round(x[i, "Percentage"], digits=0), '%', x[i, column_name], " (total:",x[i, "Freq"],")")
    actual <- actual+x[i, "Percentage"]
  }
  return(desc)
}

#' Verbal Description of a numerical feature
#'
#' @param data the data that was used in order to generate the clusters
#' @param clustering the clusters that have been assigned to the data instances
#' @param cluster_name the cluster name for which the local scores should be calculated
#' @param column_name the name of the column/feature to be described
#' @param percentage range that should be shown additionally to max and min values, e.g. .9 -> 90 percent range
#' @param num_digits rounding of digits
#'
#' @return  verbal description of a numerical feature
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' data_wo_clustering <- data[c(1,2)]
#' clustering <- data[3]
#' # generate decription of weight for cluster 1
#' cat(describe_numerical_feature(data_wo_clustering, clustering, 1, 'Weight'))
#' cat(describe_numerical_feature(data_wo_clustering, clustering, 1, 'Weight',percentage = .7 ,num_digits = 2))
describe_numerical_feature <- function(data, clustering, cluster_name, column_name,percentage = .9 ,num_digits = 1){
  data_clustered <-  data %>% mutate(Cluster = clustering)
  percentiles=c((1-percentage)/2, 1-((1-percentage)/2))
  y <- data_clustered %>% filter(Cluster == cluster_name) %>% select(column_name)
  quantiles <- quantile(y[,1], probs = percentiles)
  desc <- paste("Cluster",cluster_name,"is described by", column_name)
  desc <- paste(desc,  "\n  - Mean: ", round(mean(y[,1]), digits = num_digits))
  desc <- paste(desc,  "\n  - Median: ", round(median(y[,1]), digits = num_digits))
  desc <- paste(desc,  "\n  - Total Value Range:", min(y[,1]),'-', max((y[,1])))
  desc <- paste(desc,  "\n  -",percentage*100,'%  Value Range:', quantiles[1],'-',quantiles[2])
  return(desc)
}

#' Verbal Description of a numerical feature simplified
#' @description
#' A simple description by categorizing a feature value into 5 bins, very low, low, medium, high ,and very high
#'
#'
#' @param data the data that was used in order to generate the clusters
#' @param clustering the clusters that have been assigned to the data instances
#' @param cluster_name the cluster name for which the local scores should be calculated
#' @param column_name the name of the column/feature to be described
#'
#' @return simplified verbal description of a numerical feature
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' data_wo_clustering <- data[c(1,2)]
#' clustering <- data[3]
#' # simpified description of weight and cluster 1
#' cat(describe_numerical_feature_simplified(data_wo_clustering, clustering, 1, 'Weight'))
describe_numerical_feature_simplified <- function(data, clustering, cluster_name, column_name){
  max_percentage_group = .9 # 90 percent than only one value #TODO not yet implemented
  min_percentage_group = .15 # else use min and max values by one to 15
  categories <- c('very low', 'low', 'medium', 'high', 'very high')
  data_frame_binned <- data %>%
    mutate(Cluster = clustering) %>%
    mutate(bins = ntile(get(column_name), n = length(categories))) %>%
    mutate(bins = categories[bins])
  range_table <- table( data_frame_binned[data_frame_binned$Cluster == cluster_name,]['bins'])
  range_table <- range_table / sum(range_table)
  range_table <- data.frame(range_table)
  colnames(range_table) <- c('Var1', 'Freq')
  range_table <- range_table %>%
    arrange(factor(Var1, levels = categories)) %>%
    filter(Freq >= min_percentage_group)
  exp <- paste('Cluster', cluster_name, 'range of', column_name, 'is', range_table[1,1], 'to', range_table[nrow(range_table), 1])
  return(exp)
}

#' Verbal description of a feature present in a cluster
#'
#' @description
#' Generates a verbal description for a feature. Descriptions are dependent on the type of the feature.
#'
#'
#' @param data the data that was used in order to generate the clusters
#' @param clustering the clusters that have been assigned to the data instances
#' @param cluster_name the cluster name for which the local scores should be calculated
#' @param column_name the name of the column/feature to be described
#'
#' @return verbal description
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' data_wo_clustering <- data[c(1,2)]
#' clustering <- data[3]
#' # indicate which columns are categorical and which are numerical
#' numerical_col_idx <- c(2)
#' categorical_col_idx <- c(1)
#' cat(describe_feature(data_wo_clustering, clustering, categorical_col_idx, 1, 'Gender'))
#' cat(describe_feature(data_wo_clustering, clustering,categorical_col_idx, 1, 'Weight'))
describe_feature <- function(data, clustering, categorical_columns, cluster_name, column_name){
  categorical_col_names <- colnames(data)[categorical_columns]
  if(column_name %in% categorical_col_names){
    return(describe_categorical_feature(data, clustering, cluster_name, column_name))
  }
  desc <- paste(describe_numerical_feature(data, clustering, cluster_name, column_name), describe_numerical_feature_simplified(data, clustering, cluster_name, column_name), sep = '\n\n')
  return(desc)
}

