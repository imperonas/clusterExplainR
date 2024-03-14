#' Sloppy Binwidth implementation
#' @description
#' Generates a unified separation in bins for all kinds of numeric variables.
#'
#' @param min Minimum value
#' @param max Maximum value
#'
#' @return binwidth
#' @noRd
calculate_binwidth <- function(min, max){
  if ((max-min)< 5){
    return((max - min)/15)
  }
  if ((max-min) < 15){
    return(1)
  }
  if((max-min) < 100){
    return(round((max - min)/15, digits = 0))
  }
  return(round((max - min)/15, digits=0) +1)
}

# requires the Column of clustering to be named Cluster
display_column_dist_by_cluster <- function(data_clustered, column_name, categorical_columns){
  p <- ggplot(data_clustered, aes(x =get(column_name), color=factor(Cluster), fill=factor(Cluster)))
  if(column_name %in% colnames(data_clustered)[categorical_columns]){
    p <- p+
      geom_bar(position = "dodge2")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.7, size = 6, hjust = 1))
  } else {
    p <- p +
      geom_density(alpha=.3)

  }
  p <- p +
    labs(title = 'Distributions over all Clusters', subtitle = "Poses a first overview", x=column_name , y="Total/Density")
  return(p)
}
display_colum_cluster_vs_population <-function(data_clustered,categorical_columns, cluster_name, column_name, binwidth){
  if(missing(binwidth)) {
    if(!(column_name %in% colnames(data_clustered)[categorical_columns])){
      min = min(data_clustered %>% select(column_name))
      max = max(data_clustered %>% select(column_name))
      binwidth <- calculate_binwidth(min,max)
    }
  }
  p <-ggplot(NULL, aes(x=get(column_name)))
  if(column_name %in% colnames(data_clustered)[categorical_columns]){
    p <- p +
      geom_bar(data = data_clustered, fill='gray', position = "dodge") +
      geom_bar(data = data_clustered[data_clustered$Cluster == cluster_name,], fill = 'red', position = "dodge", width = .85)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.7,size = 6, hjust = 1))
  } else {
    p <- p +
      geom_histogram(data = data_clustered, fill='gray', position = "dodge", binwidth = binwidth) +
      geom_histogram(data = data_clustered[data_clustered$Cluster == cluster_name,], fill = 'red', position = "dodge", binwidth = binwidth)
  }
  return(p)
}


#' Build a tree that represents multiple clusters
#'
#' @description
#' Recursive function that builds a tree from cluster description rules.
#' Since the rules that describe a cluster are applied in conjunction the order of the rules can be changed.
#' Based on this idea, a tree can be build by taking the most prominent feature at any point in time and create a new subtree.
#'
#'
#' @param node a node of type data.tree Node
#' @param clusters a list of cluster rules, as returned from the explainer
#' @param used_features features already used in parent tree
#'
#' @return prototype of rule tree
#' @noRd
#'
rec_tree_build <- function(node, clusters, used_features){
  print(paste('Called with root:', node$name, '-- and ', length(clusters), 'clusters'))
  print(used_features)
  # base case: one cluster only -> append all rules for not previously used features
  if (length(clusters)==1){
    ref <- node
    for(rule in clusters[[1]]$rule$rules){
      if(rule$column %!in% used_features){
        ref <- ref$AddChild(rule$verbalize)
      }
    }
    ref$AddChild(paste('Cluster',  clusters[[1]]$cluster))
  }
  # recursive case:
  else {
    # for all rules, off all clusters, count the most prominent column, not yet used
    all_features <- sapply(sapply(clusters, function(c) c$rule$rules), function(r) r$column)
    all_features <- c()
    for (cluster in clusters) {
      columns <- sapply(cluster$rule$rules, function(r) r$column)
      all_features <- c(all_features,columns)
    }
    print('all features')
    print(all_features)
    all_possible_features <- all_features[all_features %!in% used_features]
    print('possible features')
    print(all_possible_features)
    # TODO this seems to be broken
    most_commmon_feature_tmp <- data.table(table(all_possible_features), stringsAsFactors = FALSE) %>% arrange(desc(N))
    print(most_commmon_feature_tmp)
    most_commmon_feature<- most_commmon_feature_tmp[1,1]
    print(paste('Most common feature', most_commmon_feature))
    # for detected column, -> select all clusters that contain those rules
    clusters_with_feature <- list()
    clusters_without_feature <- list()
    for (cluster in clusters) {
      if(most_commmon_feature %in% sapply(cluster$rule$rules,function(r) r$column)){
        clusters_with_feature[[length(clusters_with_feature) +1 ]] <- cluster
      }
      else {
        clusters_without_feature[[length(clusters_without_feature) +1 ]] <- cluster
      }
    }

    print(paste('Clusters with', sapply(clusters_with_feature, function(c) c$cluster)))
    print(paste('Clusters without', sapply(clusters_without_feature, function(c) c$cluster)))

    # split the detected clusters into subgroups based on values
    individual_feature_rules <- list()
    splits_by_individual_rules <- list()
    for (cluster in clusters_with_feature) {
      # select the fitting rule
      # TODO why do you fail now.....
      rules_bycolumn <- sapply(cluster$rule$rules,function(r) r$column)
      rule <- cluster$rule$rules[[which(as.character(rules_bycolumn) == as.character(most_commmon_feature))]]
      if(rule$verbalize %in% individual_feature_rules){
        idx <- which(individual_feature_rules == rule$verbalize)
        splits_by_individual_rules[[idx]][[length(splits_by_individual_rules[[idx]])+1]] <- cluster
        print(paste('added', cluster$column, 'at position:', idx, 'new length of clusters:', length(splits_by_individual_rules[[idx]])))
      }
      else{
        idx <- length(individual_feature_rules)+1
        individual_feature_rules[[idx]] <- rule$verbalize
        splits_by_individual_rules[[idx]] <- list()
        splits_by_individual_rules[[idx]][[1]] <- cluster
        print(paste('added', cluster$column, 'at position:', idx))
      }
    }
    print(individual_feature_rules)
    print(length(splits_by_individual_rules))
    # for each subgroup:
    # add a new childnode to node
    # call recursion on childnode, and set of alrady used columns (used columns + now used column)
    for (x in 1:length(individual_feature_rules)) {
      new_node <- node$AddChild(individual_feature_rules[[x]])
      used_features <- c(used_features,c(most_commmon_feature))
      rec_tree_build(new_node,splits_by_individual_rules[[x]], used_features)
    }
    # recursively recall same node with the clusters not contained in selected feature
    if(length(clusters_without_feature)>0){
      rec_tree_build(node,clusters_without_feature, used_features)
    }
  }
}

plot_top_n_most_important_features <- function(data, clustering, numerical_columns, categorical_columns, cluster, feature_importance, num_features){
  plotlist = {}
  ranked_features_for_cluster <- feature_importance %>%
    arrange(desc(Importance_score)) %>%
    filter(Cluster_id == cluster)
  for (x in 1:num_features) {
    column_name <-ranked_features_for_cluster[[2]][x]
    importance_score <- ranked_features_for_cluster[[3]][x]
    verbalised_description <- describe_feature(data, clustering, categorical_columns, cluster, column_name)
    # create a plot for a specific feature
    p <- display_colum_cluster_vs_population(data %>% mutate(Cluster=clustering),categorical_columns, cluster, column_name) +
      labs(subtitle = paste(x,':', column_name,'\n   ', round(importance_score, digits=2)) , title = NULL, x = NULL, y = NULL) +
      theme(legend.position="none")
    p <- annotate_figure(p,bottom = textGrob(verbalised_description, gp = gpar(cex =1-(.15*num_features))))
    plotlist[[x]] <- p
  }
  plot<- ggarrange(plotlist = plotlist,
                   align ='v',# 'hv', # v makes them same width h makes plots same height (not including the labels)
                   common.legend = TRUE,
                   legend = "bottom",
                   nrow=1)
  plot <- annotate_figure(plot,
                          #left = textGrob("Total/Density", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                          top =  textGrob(paste('Cluster:', cluster), gp = gpar(cex = 1.3)))
  return(plot)
}

display_top_n_global_features <- function(data_clustered, categorical_columns, overall_ranked_features, number_of_features, type = 'mean'){
  column_names <- list()
  plotlist <- list()
  # ordering the Columns according to correct importance score
  if(type == 'mean'){
    overall_ranked_features <- overall_ranked_features %>% arrange(desc(mean_Importance_score))
  }
  if(type == 'max'){
    overall_ranked_features <- overall_ranked_features %>% arrange(desc(max_Importance_score))
  }
  if(type == 'min'){
    overall_ranked_features <- overall_ranked_features %>% arrange(desc(min_Importance_score))
  }
  for (x in 1:min(number_of_features, nrow(overall_ranked_features))) {
    column_name <-overall_ranked_features[[1]][x]
    p <- display_column_dist_by_cluster(data_clustered, column_name, categorical_columns)
    p <- p +
      labs(subtitle = paste(column_name,'( ranked top',x,')'), title = NULL, x = NULL, y = NULL) +
      theme(legend.position="none")
    plotlist[[x]]<- p
    column_names[x]<- column_name
  }
  plot<- ggarrange(plotlist = plotlist,
                   align ='v',# 'hv', # v makes them same width h makes plots same height (not including the labels)
                   common.legend = TRUE,
                   legend = "bottom")
  plot <- annotate_figure(plot, left = textGrob("Total/Density", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                          top =  textGrob("Overall most important features", gp = gpar(cex = 1.3)))
  return(plot)
}

