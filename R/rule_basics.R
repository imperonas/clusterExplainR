#' Categorical Rule
#'
#' @param column_name Name of the column for the Rule
#' @param values list of categorical values that satisfy the Rule
#'
#' @return
#' A rule on categorical features that can be applied to a dataframe:
#'  \item{type}{The type of the rule}
#'  \item{column}{The column name the rule is applied to}
#'  \item{values}{The values that satisfy the rule}
#'  \item{apply(dataframe)}{unction to apply the defined rule to a dataframe. Returns a dataframe with objects only satisfying the rule}
#'  \item{verbalize}{verbalization (Human readable representation) of the rule}
#' @keywords internal
#' @export
#' @examples
#' test <- data.frame(cat = c('a','b','c','a','b','c'), num = c(1,1,2,2,3,3), cluster=c(1,2,1,2,1,2))
#' cat_rule <- categoric_feature_rule('cat', c('a'))
#' print(cat_rule$verbalize)
#' cat_rule$apply(test)
categoric_feature_rule <- function(column_name, values){
  nc <- list()
  nc$type <- 'Rule for a categoric feature'
  nc$column <- column_name
  nc$values <- values
  nc$apply <- function(df){
    return(df%>% filter(get(nc$column) %in% nc$values))
  }
  nc$verbalize <- paste(column_name, 'is ', paste( unlist(values), collapse=' or '),'')
  return(nc)
}

#' Numeric Rule
#'
#' @param column_name Name of the column for the Rule
#' @param min_val the minimum that is included in the rule
#' @param max_val the maximum that is included in the rule
#'
#' @return
#' A rule on numeric features that can be applied to a dataframe:
#'  \item{type}{The type of the rule}
#'  \item{column}{The column name the rule is applied to}
#'  \item{min}{The min value that satisfy the rule}
#'  \item{max}{The min value that satisfy the rule}
#'  \item{apply(dataframe)}{Function to apply the defined rule to a dataframe. Returns a dataframe with objects only satisfying the rule}
#'  \item{verbalize}{Verbalization (Human readable representation) of the rule}
#' @keywords internal
#' @export
#' @examples
#' test <- data.frame(cat = c('a','b','c','a','b','c'), num = c(1,1,2,2,3,3), cluster=c(1,2,1,2,1,2))
#' num_rule <- numeric_feature_rule('num',1,1)
#' print(num_rule$verbalize)
#' num_rule$apply(test)
numeric_feature_rule <- function(column_name, min_val,max_val){
  nc <- list()
  nc$type <- 'Rule for a numeric feature'
  nc$column <- column_name
  nc$min <- min_val
  nc$max <- max_val
  nc$apply <- function(df){
    return(df %>% filter(get(nc$column) >= min_val) %>% filter(get(nc$column) <= max_val))
  }
  nc$verbalize <- paste(column_name, 'is between ', min_val, 'and', max_val)
  return(nc)
}



#' Conjunctinoal Rule Chaining
#'@description
#'Combining multiple rules either numerical or categorical or combined into a single rule using conjunction
#'
#'
#' @param rules a set of rules that should be combined in conjunction
#'
#' @return
#' A rule composed of multiple rules, consisting of:
#'  \item{type}{The type of the rule}
#'  \item{rules}{A list of the individual rules that were combined}
#'  \item{apply(dataframe)}{Function to apply the combined rule to a dataframe. Returns a dataframe with objects only satisfying the rule}
#'  \item{verbalize}{Verbalization of the rule (Human readable representation)}
#' @keywords internal
#' @export
#' @examples
#' test <- data.frame(cat = c('a','b','c','a','b','c'), num = c(1,1,2,2,3,3), cluster=c(1,2,1,2,1,2))
#' cat_rule <- categoric_feature_rule('cat', c('a'))
#' num_rule <- numeric_feature_rule('num',1,1)
#' rules <- list(cat_rule, num_rule)
#' chain_rules_conjunction(rules)$apply(test)
chain_rules_conjunction <- function(rules){
  nc <- list()
  nc$type <- 'Conjunction of Rules'
  nc$rules <- rules
  nc$apply <- function(df){
    d <- df
    for (rule in rules) {
      d <- rule$apply(d)
    }
    return(d)
  }
  nc$verbalize <- paste( unlist(sapply(nc$rules, "[[", 'verbalize')), collapse='\n AND ')
  return(nc)
}

#' Disjunctional Rule Chaining
#'@description
#'Combining multiple rules either numerical or categorical or combined into a single rule using disjunction
#'
#'
#' @param rules a set of rules that should be combined in disjunction
#'
#' @return
#' A rule composed of multiple rules:
#'  \item{type}{The type of the rule}
#'  \item{rules}{A list of the individual rules that were combined}
#'  \item{apply(dataframe)}{Function to apply the combined rule to a dataframe. Returns a dataframe with objects only satisfying the rule}
#'  \item{verbalize}{Verbalizationof the rule (Human readable representation)}
#' @keywords internal
#' @export
#' @examples
#' test <- data.frame(cat = c('a','b','c','a','b','c'), num = c(1,1,2,2,3,3), cluster=c(1,2,1,2,1,2))
#' cat_rule <- categoric_feature_rule('cat', c('a'))
#' num_rule <- numeric_feature_rule('num',1,1)
#' rules <- list(cat_rule, num_rule)
#' chain_rules_disjunction(rules)$apply(test)
chain_rules_disjunction <- function(rules){
  nc <- list()
  nc$type <- 'Disjunction of Rules'
  nc$rules <- rules
  nc$apply <- function(df){
    d <- list()
    for (rule in rules) {
      index <- length(d) +1
      d[[index]] <- rule$apply(df)
    }
    return(Reduce(function(x, y) merge(x, y, all=TRUE), d))
  }
  nc$verbalize <- paste( unlist(sapply(nc$rules, "[[", 'verbalize')), collapse='\n OR ')
  return(nc)
}

#' Evaluate Conjunction of Rules
#'
#' @description
#' Evaluates how well a set of rules predicts a certain target. Works as follows: First Rule is evaluated
#' in terms of Accuracy and Coverage using TP,FP,TN and FN.
#' The next rule from the rule set is added in conjunction and evaluated until no more rules are left.
#' However, if ever an accuracy of 1.0 is reached the evaluation terminates since the target is already optimally predicted
#'
#'
#' @param data some dataframe on which the rules should be evaluated
#' @param rules a set of rules that should be evaluated
#' @param target_column name of the column that should be predicted
#' @param target the target value of the target column that should be predicted
#'
#' @return a dataframe that consists of:
#'  \item{Rule}{The verbalization of the rule, added latest}
#'  \item{Accuracy}{The Accuracy of the Rule, how many instances are correctly predicted}
#'  \item{Coverage}{Coverage of the Rule, how much the rule covers the targeted instances}
#'  \item{TP}{True Positive predictions}
#'  \item{FP}{False Positive predictions}
#'  \item{TN}{True Negative predictions}
#'  \item{FN}F{alse Negative predictions}
#' @keywords internal
#' @export
#'
#' @examples
#' test <- data.frame(cat = c('a','b','c','a','b','c'), num = c(1,1,2,2,3,3), cluster=c(1,2,1,2,1,2))
#' cat_rule <- categoric_feature_rule('cat', c('a'))
#' num_rule <- numeric_feature_rule('num',1,1)
#' rules <- list(cat_rule, num_rule)
#' evaluate_rule_application_in_conjunction(test,rules,'cluster',1)
evaluate_rule_application_in_conjunction <- function(data,rules, target_column, target){
  cp <- data
  total_number_targets = nrow(data %>% filter(get(target_column) == target))
  df = data.frame(
    Rule=character(),
    Accuracy=double(),
    Coverage=double(),
    TP=integer(),
    FP=integer(),
    TN=integer(),
    FN=integer(),
    stringsAsFactors=FALSE)
  for (rule in rules) {
    number_columns_before_rule_application = nrow(cp)
    # all negatives
    N = nrow(cp %>% filter(get(target_column) != target))
    P = nrow(cp %>% filter(get(target_column) == target))
    # first apply the rule
    cp = rule$apply(cp)
    # calculate the coverage TP/total_number_target
    TP = nrow(cp %>% filter(get(target_column) == target))
    coverage = TP/total_number_targets
    # calculate accuracy (TP+TN)/ncol(cp)
    TN = N -nrow(cp %>% filter(get(target_column) != target))
    accuracy = (TP+TN)/number_columns_before_rule_application
    df <- rbind(df, list(rule$verbalize, accuracy, coverage, TP, P-TP, TN, N-TN))
    # if we have a total accurate rule there is no more need to go any further
    if (accuracy >= 1.0) {
      break
    }
  }
  colnames(df)<- c('Rule', 'Accuracy', 'Coverage', 'TP', 'FP', 'TN', 'FN')
  return(df)
}


#' Rule Generation based on F1
#' @description
#' Find the best possible rule for a given feature to predict a specific cluster, using the F1 score to evaluate possible rules.
#' For categorical features all permutations of possible values that can predict the cluster are used and the best one is selected.
#' For numerical features all possible ranges in 5% steps are evaluated.
#' The rules are selected based in the F1 Score in order to balance accuracy and coverage by using the harmonic mean of precision and recall (F1).
#' @param data the data that was used in order to generate the clusters
#' @param clustering the clusters that have been assigned to the data instances
#' @param categoric_columns all column indexes that are of categorical/binary/ordinal type
#' @param column_name the column that should be used to create the optimal rule
#' @param cluster_name the name of the cluster that should be predicted
#' @return a rule either categorical or numerical depending of the type of the feature
#' @keywords internal
#' @export
#'
#' @examples
#' data <- clusterExplainR::example_mocked_clustering
#' clustering <- data$Cluster
#' rule_cat <- generate_rule_based_on_F1(data[c(1,2)],clustering,c(1),'Gender',1)
#' rule_cat$verbalize
#' rule_num <- generate_rule_based_on_F1(data[c(1,2)],clustering,c(1),'Weight',1)
#' rule_num$verbalize
generate_rule_based_on_F1 <- function(data, clustering, categoric_columns, column_name, cluster_name){
  data_clustered <- data %>% mutate(Cluster = clustering)
  cp <- copy(data_clustered)
  P  <- nrow(cp %>% filter(Cluster == cluster_name)) # total positives
  N  <- nrow(cp %>% filter(Cluster != cluster_name)) # total negatives
  best_rule <- NA
  best_f1 <- 0
  rules <- list()
  # if categorical column <- create all possible combinations of unique values, for all of them create rules
  categorical_col_names <- colnames(cp)[categoric_columns]
  if(column_name %in% categorical_col_names){
    # get the unique values for the Cluster
    cluster_values <- cp %>% filter(Cluster == cluster_name) %>% select(column_name) %>% distinct()
    cluster_values <- cluster_values[,column_name]
    for (x in 1:length(cluster_values)) {
      combination = combn(cluster_values,x)
      for (col in 1:ncol(combination)) {
        rules [[length(rules)+1]]= categoric_feature_rule(column_name, combination[,col])
      }
    }
  }
  # if numeric colum <- create rule based on 5 percent increments, and create all of the rules
  else{
    cluster_values <- cp%>% filter(Cluster == cluster_name)  %>% select(column_name)
    # calculate all 5% steps
    test_ranges = 0:20 * 0.05 * (max(cluster_values)-min(cluster_values)) + min(cluster_values)
    for (idx_min in 1:(length(test_ranges)-1)) {
      for (idx_max in (idx_min+1):length(test_ranges)) {
        rules [[length(rules)+1]]= numeric_feature_rule(column_name, test_ranges[idx_min],test_ranges[idx_max])
      }
    }
  }
  for (rule in rules) {
    rule_result <- rule$apply(cp)
    TP = nrow(rule_result %>% filter(Cluster == cluster_name))
    FP = P - TP
    FN = nrow(rule_result %>% filter(Cluster != cluster_name))
    TN = N - FN
    F1 = TP/(TP+0.5*(FP+FN))
    if (F1 > best_f1) {
      best_f1 = F1
      best_rule = rule
    }
  }
  return(best_rule)
}

#' Check if a rule is contained in a list of rules
#'
#' @description
#' Checks if a rule is contained in a list of rules, by checking if the verablizations are the same.
#' This works since the verbalizations of a rule are automatically generated.
#' Does work for composite rules but order matters.
#'
#' @param rule a rule as implemented
#' @param rule_list a list of rules
#'
#' @return Boolean
#' @keywords internal
#' @export
#'
#' @examples
#' cat_rule <- categoric_feature_rule('cat', c('a'))
#' num_rule <- numeric_feature_rule('num',1,1)
#' rules <- list(cat_rule, num_rule)
#' rule_is_in_rulelist(num_rule, rules)
#' rule_is_in_rulelist(categoric_feature_rule('cat', c('b')), rules)
rule_is_in_rulelist <- function(rule, rule_list){
  for (r in rule_list) {
    if (r$verbalize == rule$verbalize) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Generate a minimal set of rules that describes the cluster
#' @description
#' Iteratively selects the best feature according to feature importance scores.
#' For this feature the best rule is selected by optimizing for the F1 Score of possible rules.
#'
#'
#' @param data the data that was used in order to generate the clusters
#' @param clustering the clusters that have been assigned to the data instances
#' @param numerical_columns all column indexes that are of continuous type
#' @param categoric_columns all column indexes that are of categorical/binary/ordinal type
#' @param cluster_name the name of the cluster that should be explained
#'
#' @return minimal set of rules:
#' \item{data}{A dataframe showing the key metrics after a new rule has been added}
#' \item{rule}{The rule as a conjunction of the rule-set}
#' \item{verbalization}{Human readable representaion of the ruls}
#' \item{cluster}{ID of cluster that the rule describes}
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
#' # generate rules that describe cluster 3
#' rule_3 <- generate_min_set_rules(data[c(1,2)], data[3],numerical_col_idx,categorical_col_idx,3)
#' cat(rule_3$verbalization)
#' rule_2 <- generate_min_set_rules(data[c(1,2)], data[3],numerical_col_idx,categorical_col_idx,2)
#' cat(rule_2$verbalization)
#' rule_1 <- generate_min_set_rules(data[c(1,2)], data[3],numerical_col_idx,categorical_col_idx,1)
#' cat(rule_1$verbalization)
generate_min_set_rules <- function(data, clustering, numerical_columns, categorical_columns, cluster_name){
  cp <- data %>% mutate(Cluster = clustering)
  total_number_targets = nrow(cp %>% filter(Cluster == cluster_name))
  total_number_of_non_targets = nrow(cp %>% filter(Cluster != cluster_name))
  df = data.frame(
    Rule=character(),
    Accuracy=double(),
    Coverage=double(),
    TP=integer(),
    FP=integer(),
    TN=integer(),
    FN=integer(),
    stringsAsFactors = FALSE)
  rules = list()
  # select most important feature
  features <- calculate_feature_importance_cluster(cp, cluster_name, numerical_columns, categorical_columns) %>% select(Column_name)
  number_of_features <- nrow(features)
  feature <- features[[1,1]]
  used_features <- list()
  while (nrow(cp)>0 & length(used_features)<number_of_features) {
    rule <- generate_rule_based_on_F1(cp[, colnames(cp)[colnames(cp) != 'Cluster']], cp$Cluster ,categorical_columns, feature, cluster_name)
    used_features[[length(used_features)+1]] <- feature
    number_columns_before_rule_application = nrow(cp)
    # all negatives
    N = nrow(cp %>% filter(Cluster != cluster_name))
    P = nrow(cp %>% filter(Cluster == cluster_name))
    # first apply the rule -- cut the dataframe
    cp = rule$apply(cp)
    # calculate the coverage TP/total_number_target
    TP = nrow(cp %>% filter(Cluster == cluster_name))
    coverage = TP/total_number_targets
    TN = N -nrow(cp %>% filter(Cluster != cluster_name))
    FN = N - TN
    # calculate accuracy related to the absolute total of elements
    # overall correct predictions when looking at complete DS
    accuracy = (TP+ (total_number_of_non_targets - FN))/(total_number_of_non_targets + total_number_targets)
    # only add a new feature if it adds discriminative value to cluster destinctions
    if (TN >0) { # here added to only keep rules that distinguish to other clusters
      df <- df %>% add_row(Rule = rule$verbalize, Accuracy = accuracy, Coverage = coverage , TP = TP, FP = P-TP, TN = TN, FN = FN)
      rules[[length(rules)+1]] <-rule
    }
    if (FN == 0) {
      # no more false predictions -- no more need to add any rules
      break
    }
    fes <- calculate_feature_importance_cluster(cp, cluster_name, numerical_columns, categorical_columns) %>% select(Column_name)
    feature <- fes[[1,1]]
  }
  colnames(df)<- c('Rule', 'Accuracy', 'Coverage', 'TP', 'FP', 'TN', 'FN')
  # generate one single rule from all rules
  single_rule <- chain_rules_conjunction(rules)
  # create a verbalization
  verbalization <- paste(unlist(df$Rule), collapse = '\n AND ')
  # and add accuracy and coverage
  verbalization <- paste('Rule:',cluster_name, '( Acc:',df[nrow(df),2]*100,'Cov:',df[nrow(df),3]*100,', in %)\n',verbalization)
  return(list(data=df, rule = single_rule, verbalization = verbalization, cluster = cluster_name))
}
