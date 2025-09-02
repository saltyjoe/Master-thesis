# 加载必要的包
library(readxl)  # 用于读取Excel文件
library(dplyr)   # 用于数据处理和连接

# 设置工作目录 - 请替换为您的文件路径
# setwd("C:/Your/Path/Here")  # 取消注释并设置您的路径

# 读取两个Excel文件
# 请替换文件路径为您的实际路径
final_compustat <- read_excel("E:/me/RSM/thesis/results/final_compustat.xlsx")
document_topic_distribution <- read.csv("E:/me/RSM/thesis/results/document_topic_distribution.csv")

# 查看数据结构
str(final_compustat)
str(document_topic_distribution)

# 确保uniqueid列在两个数据框中都存在
if(!("uniqueid" %in% names(final_compustat)) || !("uniqueid" %in% names(document_topic_distribution))) {
  stop("uniqueid列在一个或两个数据框中不存在")
}

# 查看两个数据集的uniqueid列的交集大小
uniqueid_intersection <- intersect(
  final_compustat$uniqueid, 
  document_topic_distribution$uniqueid
)
cat("交集中的uniqueid数量:", length(uniqueid_intersection), "\n")

# 使用左连接合并数据集（保留两边所有数据）
merged_data <- inner_join(
  final_compustat, 
  document_topic_distribution, 
  by = "uniqueid"
)

# 查看合并后的数据结构
str(merged_data)

# 显示合并后的前几行
head(merged_data)

# 保存合并后的数据为新的Excel文件
# 取消下面的注释并设置您的文件路径
# write.csv(merged_data, "merged_data.csv", row.names = FALSE)
# 或者保存为Excel
library(writexl)
write_xlsx(merged_data, "E:/me/RSM/thesis/results/merged_data.xlsx")

#---------------------------------------
# XGboosting
#---------------------------------------
#install.packages("SHAPforxgboost")
library(SHAPforxgboost)
library(ggplot2)
library(dplyr)
library(reshape2)
library(pROC)
library(caret)
library(xgboost)

merged_data <- read_excel("E:/me/RSM/thesis/data/results/merged_clean.xlsx")

# 定义财务变量和主题变量
financial_vars <- c(
  "ROA", "ROE", "EBIT_Margin", "Gross_Margin", "CFO_to_Assets", 
  "Accruals", "Current_Ratio", "Quick_Ratio", "Cash_Ratio", 
  "Debt_to_Equity", "Debt_to_Assets", "Long_Term_Debt_to_Assets", 
  "Asset_Turnover", "Inventory_Turnover", "Receivables_Turnover", 
  "Market_Cap", "Book_Value", "Market_to_Book", "Price_to_Earnings", 
  "Dividend_Yield", "CAPEX_to_Assets"
)

# 假设您有40个主题，可以根据实际情况调整
num_topics <- 140
topic_vars <- paste0("Topic_", 1:num_topics)

# 合并所有变量
all_vars <- c(financial_vars, topic_vars)

# Split into training and testing sets
# Use a time-based split as mentioned in your proposal
train_data <- merged_data[merged_data$fyear %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2019,2020,2021), ]
test_data <- merged_data[merged_data$fyear %in% c(2022,2023), ]

# 确认目标变量名称是eps_increase而不是earnings_increase
target_var <- "eps_increase"

# 输出数据集大小
cat("训练集大小:", nrow(train_data), "行\n")
cat("测试集大小:", nrow(test_data), "行\n")

# 数据预处理函数
preprocess_data <- function(data, vars, target) {
  # 检查并处理缺失值
  missing_count <- colSums(is.na(data[, vars]))
  vars_with_missing <- names(missing_count[missing_count > 0])
  
  if(length(vars_with_missing) > 0) {
    cat("处理缺失值的变量:", paste(vars_with_missing, collapse=", "), "\n")
    for(var in vars_with_missing) {
      data[[var]][is.na(data[[var]])] <- median(data[[var]], na.rm=TRUE)
    }
  }
  
  # 检查并处理无限值
  for(var in vars) {
    inf_indices <- is.infinite(data[[var]])
    if(any(inf_indices)) {
      cat(var, "存在无限值，进行处理\n")
      finite_vals <- data[[var]][is.finite(data[[var]])]
      max_val <- max(finite_vals, na.rm=TRUE)
      min_val <- min(finite_vals, na.rm=TRUE)
      data[[var]][data[[var]] == Inf] <- max_val
      data[[var]][data[[var]] == -Inf] <- min_val
    }
  }
  
  # 确保目标变量是因子型
  data[[target]] <- as.factor(data[[target]])
  
  return(data)
}

# 预处理训练集和测试集
train_data <- preprocess_data(train_data, all_vars, target_var)
test_data <- preprocess_data(test_data, all_vars, target_var)

# 计算F1分数的函数
calc_f1 <- function(actual, predicted) {
  confusion <- table(Actual = actual, Predicted = predicted)
  
  # 检查混淆矩阵维度，确保为2x2
  if(nrow(confusion) != 2 || ncol(confusion) != 2) {
    # 如果缺少某个类别，手动扩展混淆矩阵
    full_confusion <- matrix(0, nrow=2, ncol=2)
    rownames(full_confusion) <- c("0", "1")
    colnames(full_confusion) <- c("0", "1")
    
    for(i in rownames(confusion)) {
      for(j in colnames(confusion)) {
        full_confusion[i, j] <- confusion[i, j]
      }
    }
    confusion <- full_confusion
  }
  
  tp <- confusion["1", "1"]
  fp <- confusion["0", "1"]
  fn <- confusion["1", "0"]
  
  # 避免除以零
  if(tp == 0) return(0)
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  
  if(precision + recall == 0) return(0)
  f1 <- 2 * precision * recall / (precision + recall)
  
  return(f1)
}

# 评估模型的函数
evaluate_model <- function(model, test_data, features, target) {
  # 创建DMatrix对象
  dtest <- xgb.DMatrix(as.matrix(test_data[, features]), label = as.numeric(test_data[[target]]) - 1)
  
  # 预测概率
  pred_prob <- predict(model, dtest)
  
  # 转换为二分类预测
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  # 实际值
  actual <- as.numeric(test_data[[target]]) - 1
  
  # 计算准确率
  accuracy <- mean(pred_class == actual)
  
  # 计算AUC
  roc_obj <- roc(actual, pred_prob, quiet = TRUE)
  auc_val <- auc(roc_obj)
  
  # 计算F1分数
  f1 <- calc_f1(actual, pred_class)
  
  # 计算混淆矩阵
  conf_matrix <- table(Actual = actual, Predicted = pred_class)
  
  # 返回评估结果
  list(
    accuracy = accuracy,
    auc = auc_val,
    f1 = f1,
    conf_matrix = conf_matrix
  )
}

#######################################################
# 1. 构建和评估基准模型（仅使用财务变量）
#######################################################

# 准备XGBoost数据集
dtrain_baseline <- xgb.DMatrix(
  data = as.matrix(train_data[, financial_vars]), 
  label = as.numeric(train_data[[target_var]]) - 1
)

# 使用交叉验证进行网格搜索调参
nrounds_values <- c(100, 200, 300, 500, 1000)
max_depth_values <- c(2, 4, 7, 9, 12)
eta_values <- c(0.01, 0.05, 0.1, 0.2)
subsample_values <- c(0.6, 0.7, 0.8, 0.9, 1.0)
colsample_bytree_values <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

cat("开始基准模型网格搜索调参...\n")

# 初始化最佳参数和性能
best_params_baseline <- NULL
best_cv_score_baseline <- 0

# 使用简化的网格搜索以节省时间
for(nrounds in nrounds_values) {
  for(max_depth in max_depth_values) {
    for(eta in eta_values) {
      # 固定其他参数以减少计算量
      subsample <- 0.8
      colsample_bytree <- 0.8
      
      params <- list(
        objective = "binary:logistic",
        eval_metric = "auc",
        max_depth = max_depth,
        eta = eta,
        subsample = subsample,
        colsample_bytree = colsample_bytree
      )
      
      # 5折交叉验证
      cv_result <- xgb.cv(
        params = params,
        data = dtrain_baseline,
        nrounds = nrounds,
        nfold = 5,
        early_stopping_rounds = 10,
        verbose = 0
      )
      
      # 获取最佳AUC分数和对应的迭代次数
      best_iteration <- which.max(cv_result$evaluation_log$test_auc_mean)
      cv_score <- cv_result$evaluation_log$test_auc_mean[best_iteration]
      
      # 如果这组参数达到更好的AUC，则更新最佳参数
      if(cv_score > best_cv_score_baseline) {
        best_cv_score_baseline <- cv_score
        best_params_baseline <- list(
          nrounds = best_iteration,
          max_depth = max_depth,
          eta = eta,
          subsample = subsample,
          colsample_bytree = colsample_bytree
        )
      }
    }
  }
}

cat("基准模型最佳参数:\n")
print(best_params_baseline)

# 使用最佳参数训练基准模型
baseline_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = best_params_baseline$max_depth,
  eta = best_params_baseline$eta,
  subsample = best_params_baseline$subsample,
  colsample_bytree = best_params_baseline$colsample_bytree
)

baseline_model <- xgboost(
  params = baseline_params,
  data = dtrain_baseline,
  nrounds = best_params_baseline$nrounds,
  verbose = 0
)

# 评估基准模型
baseline_eval <- evaluate_model(baseline_model, test_data, financial_vars, target_var)

cat("基准模型性能评估结果:\n")
cat("Accuracy:", round(baseline_eval$accuracy, 4), "\n")
cat("AUC:", round(baseline_eval$auc, 4), "\n")
cat("F1 Score:", round(baseline_eval$f1, 4), "\n")
cat("混淆矩阵:\n")
print(baseline_eval$conf_matrix)

#######################################################
# 2. 构建和评估增强模型（财务变量 + 主题变量）
#######################################################

# 准备XGBoost数据集
dtrain_enhanced <- xgb.DMatrix(
  data = as.matrix(train_data[, all_vars]), 
  label = as.numeric(train_data[[target_var]]) - 1
)

cat("开始增强模型网格搜索调参...\n")

# 初始化最佳参数和性能
best_params_enhanced <- NULL
best_cv_score_enhanced <- 0

# 使用简化的网格搜索以节省时间
for(nrounds in nrounds_values) {
  for(max_depth in max_depth_values) {
    for(eta in eta_values) {
      # 固定其他参数以减少计算量
      subsample <- 0.8
      colsample_bytree <- 0.8
      
      params <- list(
        objective = "binary:logistic",
        eval_metric = "auc",
        max_depth = max_depth,
        eta = eta,
        subsample = subsample,
        colsample_bytree = colsample_bytree
      )
      
      # 5折交叉验证
      cv_result <- xgb.cv(
        params = params,
        data = dtrain_enhanced,
        nrounds = nrounds,
        nfold = 5,
        early_stopping_rounds = 10,
        verbose = 0
      )
      
      # 获取最佳AUC分数和对应的迭代次数
      best_iteration <- which.max(cv_result$evaluation_log$test_auc_mean)
      cv_score <- cv_result$evaluation_log$test_auc_mean[best_iteration]
      
      # 如果这组参数达到更好的AUC，则更新最佳参数
      if(cv_score > best_cv_score_enhanced) {
        best_cv_score_enhanced <- cv_score
        best_params_enhanced <- list(
          nrounds = best_iteration,
          max_depth = max_depth,
          eta = eta,
          subsample = subsample,
          colsample_bytree = colsample_bytree
        )
      }
    }
  }
}

cat("增强模型最佳参数:\n")
print(best_params_enhanced)

# 使用最佳参数训练增强模型
enhanced_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = best_params_enhanced$max_depth,
  eta = best_params_enhanced$eta,
  subsample = best_params_enhanced$subsample,
  colsample_bytree = best_params_enhanced$colsample_bytree
)

enhanced_model <- xgboost(
  params = enhanced_params,
  data = dtrain_enhanced,
  nrounds = best_params_enhanced$nrounds,
  verbose = 0
)

# 评估增强模型
enhanced_eval <- evaluate_model(enhanced_model, test_data, all_vars, target_var)

cat("增强模型性能评估结果:\n")
cat("Accuracy:", round(enhanced_eval$accuracy, 4), "\n")
cat("AUC:", round(enhanced_eval$auc, 4), "\n")
cat("F1 Score:", round(enhanced_eval$f1, 4), "\n")
cat("混淆矩阵:\n")
print(enhanced_eval$conf_matrix)



#--------------------------------------------------------
# 时间序列交叉验证函数for XGBoost
time_series_cv_xgb <- function(dtrain, params, nrounds, nfolds = 5) {
  # 获取训练数据大小
  n_obs <- nrow(dtrain)
  
  # 生成时间序列foldid
  foldid <- generate_time_series_foldid(n_obs, nfolds)
  
  # 存储每一折的结果
  cv_scores <- numeric(nfolds)
  
  for(fold in 1:nfolds) {
    # 分割数据
    val_indices <- which(foldid == fold)
    train_indices <- which(foldid != fold)
    
    # 创建训练和验证集
    dtrain_fold <- slice(dtrain, train_indices)
    dval_fold <- slice(dtrain, val_indices)
    
    # 训练模型
    watchlist <- list(train = dtrain_fold, eval = dval_fold)
    
    model <- xgb.train(
      params = params,
      data = dtrain_fold,
      nrounds = nrounds,
      watchlist = watchlist,
      early_stopping_rounds = 10,
      verbose = 0
    )
    
    # 在验证集上预测
    pred_prob <- predict(model, dval_fold)
    true_labels <- getinfo(dval_fold, "label")
    
    # 计算AUC
    roc_obj <- roc(true_labels, pred_prob, quiet = TRUE)
    cv_scores[fold] <- auc(roc_obj)
  }
  
  return(list(
    mean_auc = mean(cv_scores),
    std_auc = sd(cv_scores),
    cv_scores = cv_scores
  ))
}

#######################################################
# 1. 构建和评估基准模型（仅使用财务变量）
#######################################################

# 准备XGBoost数据集
dtrain_baseline <- xgb.DMatrix(
  data = as.matrix(train_data[, financial_vars]), 
  label = as.numeric(train_data[[target_var]]) - 1
)

# 参数网格
nrounds_values <- c(100, 300, 500,700,1000)
max_depth_values <- c(2, 4, 6,8)
eta_values <- c(0.01, 0.05, 0.1, 0.2)
subsample_values <- c(0.6, 0.7, 0.8, 0.9, 1.0)
colsample_bytree_values <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

cat("开始基准模型网格搜索调参（时间序列交叉验证）...\n")

# 初始化最佳参数和性能
best_params_baseline <- NULL
best_cv_score_baseline <- 0

# 使用简化的网格搜索以节省时间
for(nrounds in nrounds_values) {
  for(max_depth in max_depth_values) {
    for(eta in eta_values) {
      # 固定其他参数以减少计算量
      subsample <- 0.8
      colsample_bytree <- 0.8
      
      params <- list(
        objective = "binary:logistic",
        eval_metric = "auc",
        max_depth = max_depth,
        eta = eta,
        subsample = subsample,
        colsample_bytree = colsample_bytree
      )
      
      cat(sprintf("测试参数组合: nrounds=%d, max_depth=%d, eta=%.2f\n", 
                  nrounds, max_depth, eta))
      
      # 使用时间序列交叉验证
      cv_result <- time_series_cv_xgb(
        dtrain = dtrain_baseline,
        params = params,
        nrounds = nrounds,
        nfolds = 5
      )
      
      cv_score <- cv_result$mean_auc
      
      cat(sprintf("  时间序列CV AUC: %.4f (±%.4f)\n", 
                  cv_score, cv_result$std_auc))
      
      # 如果这组参数达到更好的AUC，则更新最佳参数
      if(cv_score > best_cv_score_baseline) {
        best_cv_score_baseline <- cv_score
        best_params_baseline <- list(
          nrounds = nrounds,
          max_depth = max_depth,
          eta = eta,
          subsample = subsample,
          colsample_bytree = colsample_bytree
        )
      }
    }
  }
}

cat("基准模型最佳参数 (基于时间序列CV AUC):\n")
print(best_params_baseline)
cat("最佳CV AUC:", best_cv_score_baseline, "\n")

# 使用最佳参数训练基准模型
baseline_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = best_params_baseline$max_depth,
  eta = best_params_baseline$eta,
  subsample = best_params_baseline$subsample,
  colsample_bytree = best_params_baseline$colsample_bytree
)

baseline_model <- xgboost(
  params = baseline_params,
  data = dtrain_baseline,
  nrounds = best_params_baseline$nrounds,
  verbose = 0
)

# 评估基准模型
baseline_eval <- evaluate_model(baseline_model, test_data, financial_vars, target_var)

cat("基准模型性能评估结果:\n")
cat("Accuracy:", round(baseline_eval$accuracy, 4), "\n")
cat("AUC:", round(baseline_eval$auc, 4), "\n")
cat("F1 Score:", round(baseline_eval$f1, 4), "\n")
cat("混淆矩阵:\n")
print(baseline_eval$conf_matrix)

#######################################################
# 2. 构建和评估增强模型（财务变量 + 主题变量）
#######################################################

# 准备XGBoost数据集
dtrain_enhanced <- xgb.DMatrix(
  data = as.matrix(train_data[, all_vars]), 
  label = as.numeric(train_data[[target_var]]) - 1
)

cat("\n开始增强模型网格搜索调参（时间序列交叉验证）...\n")

# 初始化最佳参数和性能
best_params_enhanced <- NULL
best_cv_score_enhanced <- 0

# 使用简化的网格搜索以节省时间
for(nrounds in nrounds_values) {
  for(max_depth in max_depth_values) {
    for(eta in eta_values) {
      # 固定其他参数以减少计算量
      subsample <- 0.8
      colsample_bytree <- 0.8
      
      params <- list(
        objective = "binary:logistic",
        eval_metric = "auc",
        max_depth = max_depth,
        eta = eta,
        subsample = subsample,
        colsample_bytree = colsample_bytree
      )
      
      cat(sprintf("测试参数组合: nrounds=%d, max_depth=%d, eta=%.2f\n", 
                  nrounds, max_depth, eta))
      
      # 使用时间序列交叉验证
      cv_result <- time_series_cv_xgb(
        dtrain = dtrain_enhanced,
        params = params,
        nrounds = nrounds,
        nfolds = 5
      )
      
      cv_score <- cv_result$mean_auc
      
      cat(sprintf("  时间序列CV AUC: %.4f (±%.4f)\n", 
                  cv_score, cv_result$std_auc))
      
      # 如果这组参数达到更好的AUC，则更新最佳参数
      if(cv_score > best_cv_score_enhanced) {
        best_cv_score_enhanced <- cv_score
        best_params_enhanced <- list(
          nrounds = nrounds,
          max_depth = max_depth,
          eta = eta,
          subsample = subsample,
          colsample_bytree = colsample_bytree
        )
      }
    }
  }
}

cat("增强模型最佳参数 (基于时间序列CV AUC):\n")
print(best_params_enhanced)
cat("最佳CV AUC:", best_cv_score_enhanced, "\n")

# 使用最佳参数训练增强模型
enhanced_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = best_params_enhanced$max_depth,
  eta = best_params_enhanced$eta,
  subsample = best_params_enhanced$subsample,
  colsample_bytree = best_params_enhanced$colsample_bytree
)

enhanced_model <- xgboost(
  params = enhanced_params,
  data = dtrain_enhanced,
  nrounds = best_params_enhanced$nrounds,
  verbose = 0
)

# 评估增强模型
enhanced_eval <- evaluate_model(enhanced_model, test_data, all_vars, target_var)

cat("增强模型性能评估结果:\n")
cat("Accuracy:", round(enhanced_eval$accuracy, 4), "\n")
cat("AUC:", round(enhanced_eval$auc, 4), "\n")
cat("F1 Score:", round(enhanced_eval$f1, 4), "\n")
cat("混淆矩阵:\n")
print(enhanced_eval$conf_matrix)




#######################################################
# 3. 特征重要性分析
#######################################################

# 获取XGBoost模型的特征重要性
importance_matrix <- xgb.importance(feature_names = all_vars, model = enhanced_model)

# 将结果转换为数据框并排序
importance_df <- as.data.frame(importance_matrix)
importance_df <- importance_df[order(-importance_df$Gain), ]

print(importance_df)

# 筛选出主题变量的重要性
topic_importance <- importance_df[grepl("^Topic_", importance_df$Feature), ]

# 提取Top N个最重要的主题变量
N <- 40#可以根据需要调整
top_topics <- head(topic_importance, N)
top_var <- head(importance_df, 20)

# 可视化Top N重要主题变量
ggplot(top_topics, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = paste("Top", N, "Most Important Topic Features"),
       x = "Topic", 
       y = "Importance (Gain)") +
  theme_minimal()

ggplot(top_var, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = paste("Top", 20, "Most Important Features"),
       x = "Topic", 
       y = "Importance (Gain)") +
  theme_minimal()


# 筛选出XGBoost模型中最重要的主题
write.csv(topic_section_similarity, file.path(output_dir, "topic_section_similarity_1.csv"))
topic_section_similarity <- read.csv("E:/me/RSM/thesis/data/test run/topic_section_similarity.csv")

important_topic_indices <- as.numeric(gsub("Topic_", "", top_topics$Feature))

# 使用这些索引从topic_section_similarity矩阵中获取相应行
# 确保使用字符串进行索引匹配
important_topic_section_sim <- topic_section_similarity[as.character(important_topic_indices), , drop=FALSE]


print(rownames(topic_section_similarity))

# 对于每个重要主题，找出最相似的章节
important_topics_sections <- data.frame(
  Topic = top_topics$Feature,
  Importance = top_topics$Gain,
  TopKeywords = character(nrow(top_topics)),
  TopSection = character(nrow(top_topics)),
  SectionSimilarity = numeric(nrow(top_topics)),
  stringsAsFactors = FALSE
)

print(nrow(top_topics))

# 从之前保存的结果中获取主题关键词
topic_terms_df <- read.csv("E:/me/RSM/thesis/data/test run/topic_keywords.csv")

for (i in 1:nrow(top_topics)) {
  topic_idx <- as.numeric(gsub("Topic_", "", top_topics$Feature[i]))
  
  # 获取主题关键词
  keywords <- topic_terms_df[topic_terms_df$Topic == paste0("Topic_", topic_idx), ]
  important_topics_sections$TopKeywords[i] <- paste(keywords$Term[1:20], collapse = ", ")
  
  # 获取最相似章节
  topic_row <- which(rownames(topic_section_similarity) == paste0("Topic_", topic_idx))
  if (length(topic_row) > 0) {
    similarities <- as.numeric(topic_section_similarity[topic_row, -ncol(topic_section_similarity)])
    top_section_idx <- which.max(similarities)
    top_section <- colnames(topic_section_similarity)[top_section_idx]
    top_sim <- similarities[top_section_idx]
    
    important_topics_sections$TopSection[i] <- top_section
    important_topics_sections$SectionSimilarity[i] <- top_sim
  }
}

# 对每个章节，计算关联主题的总重要性
section_predictive_power <- data.frame(
  Section = unique(important_topics_sections$TopSection),
  TotalImportance = 0,
  NumTopics = 0,
  TopTopics = "",
  stringsAsFactors = FALSE
)

for (i in 1:nrow(section_predictive_power)) {
  section <- section_predictive_power$Section[i]
  
  # 找出与该章节关联的重要主题
  related_topics <- important_topics_sections[important_topics_sections$TopSection == section, ]
  
  # 计算总重要性和主题数量
  section_predictive_power$TotalImportance[i] <- sum(related_topics$Importance)
  section_predictive_power$NumTopics[i] <- nrow(related_topics)
  
  # 获取关联的前三个主题和关键词
  if (nrow(related_topics) > 0) {
    related_topics <- related_topics[order(-related_topics$Importance), ]
    top_3 <- min(3, nrow(related_topics))
    
    top_topics_desc <- sapply(1:top_3, function(j) {
      paste0(related_topics$Topic[j], " (", 
             round(related_topics$Importance[j], 4), "): ", 
             related_topics$TopKeywords[j])
    })
    
    section_predictive_power$TopTopics[i] <- paste(top_topics_desc, collapse = "; ")
  }
}

# 按总重要性排序
section_predictive_power <- section_predictive_power[order(-section_predictive_power$TotalImportance), ]

# 可视化章节预测力
ggplot(section_predictive_power, aes(x = reorder(Section, TotalImportance), y = TotalImportance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "10-K Sections Predictive Power for Earnings Changes",
       x = "Section", 
       y = "Total Importance (Sum of Topic Gain)") +
  theme_minimal()



# 检查矩阵结构
dim(topic_section_similarity)
head(rownames(topic_section_similarity))
head(colnames(topic_section_similarity))

# 检查内容是否为空或全0
sum(topic_section_similarity)
# 打印结果
print(important_topics_sections)

# 1. 首先，修正行索引映射
important_topic_indices <- as.numeric(gsub("Topic_", "", top_topics$Feature))

# 2. 检查这些索引是否在有效范围内
valid_indices <- important_topic_indices[important_topic_indices <= nrow(topic_section_similarity)]
if(length(valid_indices) < length(important_topic_indices)) {
  warning("一些主题索引超出了相似度矩阵的行数范围")
}

# 3. 提取相关行的章节相似度，排除第一列"Topic"
# 假设第一列是不需要的，从第二列开始才是实际章节
important_topic_section_sim <- topic_section_similarity[valid_indices, -1, drop=FALSE]

# 4. 现在为每个重要主题找出最相似的章节
important_topics_sections <- data.frame(
  Topic = top_topics$Feature[top_topics$Feature %in% paste0("Topic_", valid_indices)],
  Importance = top_topics$Gain[top_topics$Feature %in% paste0("Topic_", valid_indices)],
  TopKeywords = character(length(valid_indices)),
  TopSection = character(length(valid_indices)),
  SectionSimilarity = numeric(length(valid_indices)),
  stringsAsFactors = FALSE
)

# 5. 从主题关键词数据框中获取关键词
# 假设您有topic_keywords数据框，每列是一个主题的关键词
for(i in 1:length(valid_indices)) {
  topic_idx <- valid_indices[i]
  topic_col <- paste0("Topic_", topic_idx)
  
  # 获取主题关键词 (假设topic_keywords是一个数据框，列名为"Topic_1", "Topic_2"等)
  if(topic_col %in% colnames(topic_keywords)) {
    # 取前5个关键词
    keywords <- topic_keywords[1:5, topic_col]
    important_topics_sections$TopKeywords[i] <- paste(keywords, collapse=", ")
  }
  
  # 找出最相似的章节
  section_sim <- important_topic_section_sim[i, ]
  max_sim_idx <- which.max(section_sim)
  if(length(max_sim_idx) > 0 && !is.na(max_sim_idx)) {
    section_name <- colnames(important_topic_section_sim)[max_sim_idx]
    similarity <- section_sim[max_sim_idx]
    
    important_topics_sections$TopSection[i] <- section_name
    important_topics_sections$SectionSimilarity[i] <- similarity
  }
}

# 6. 打印结果
print(important_topics_sections)

# 1. 章节分布统计
section_counts <- table(important_topics_sections$TopSection)
section_counts_df <- as.data.frame(section_counts)
colnames(section_counts_df) <- c("Section", "Count")
section_counts_df <- section_counts_df[order(-section_counts_df$Count), ]

# 可视化章节分布
ggplot(section_counts_df, aes(x = reorder(Section, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribution of Important Topics Across 10-K Sections",
       x = "10-K Section", y = "Number of Important Topics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. 章节重要性总结
section_importance <- aggregate(Importance ~ TopSection, data = important_topics_sections, sum)
section_importance <- section_importance[order(-section_importance$Importance), ]

# 可视化章节重要性
ggplot(section_importance, aes(x = reorder(TopSection, -Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Predictive Power of 10-K Sections for Earnings Changes",
       x = "10-K Section", y = "Total Importance (Sum of Topic Gains)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 可视化每个章节关联的主题数量
ggplot(section_predictive_power, aes(x = reorder(Section, TotalImportance), y = NumTopics)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Number of Important Topics Associated with Each 10-K Section",
       x = "Section", 
       y = "Number of Associated Topics") +
  theme_minimal()

# 保存结果
write.csv(section_predictive_power, file = "section_predictive_power.csv", row.names = FALSE)

#######################################################
# 4. 按照主题类别分析特征重要性
#######################################################

# 定义主题类别映射（根据之前的分类）
cat("按照主题类别分析特征重要性...\n")
topic_categories <- list(
  "Performance" = c(3, 11, 16, 21, 59, 73, 81, 83, 100, 117),
  "Industry" = c(2, 7, 8, 9, 10, 14, 19, 22, 23, 30, 38, 40, 43, 54, 58, 62, 66, 67, 68, 70, 71, 74, 87, 89, 92, 93, 96, 108, 110, 113, 123, 126, 134, 136, 137, 138),
  "Employees" = c(33, 39, 55, 101, 112),
  "Compliance" = c(18, 42, 46, 49, 51, 56, 61, 82, 94, 102, 106, 115, 120),
  "Loans_Debt" = c(25, 27, 64, 98, 129, 130, 132, 135),
  "Operations" = c(1, 5, 15, 17, 20, 24, 31, 32, 37, 47, 76, 78, 86, 88, 103, 104, 107, 111, 116, 118, 121, 122, 124, 125, 127, 131, 140),
  "Stock" = c(35, 57, 99),
  "MA_Structure" = c(13, 34, 50, 105, 109, 119, 128, 133, 139),
  "Legal" = c(26, 28, 35, 36, 48, 52, 65, 69, 79, 80, 90, 91, 97, 114),
  "Geographic" = c(4, 6, 44, 53, 60, 75, 84),
  "Investments" = c(29, 41, 45, 77, 85, 95),
  "RD_IP" = c(12, 63, 72, 109),
  "Property" = c(41, 44, 50, 52, 119, 135)
)

# 创建映射函数，将主题编号转换为类别名称
get_topic_category <- function(topic_num) {
  for(category in names(topic_categories)) {
    if(topic_num %in% topic_categories[[category]]) {
      return(category)
    }
  }
  return("Unknown")
}

# 为每个主题偏差特征分配类别
deviation_features <- grep("Topic_\\d+_deviation", importance_matrix$Feature, value = TRUE)
category_importance <- data.frame(
  Feature = character(),
  Category = character(),
  Gain = numeric(),
  stringsAsFactors = FALSE
)

for(feature in deviation_features) {
  # 提取主题编号
  topic_num <- as.numeric(gsub("Topic_(\\d+)_deviation", "\\1", feature))
  category <- get_topic_category(topic_num)
  
  # 获取该特征的重要性值
  gain <- importance_matrix$Gain[importance_matrix$Feature == feature]
  
  # 添加到数据框
  category_importance <- rbind(
    category_importance,
    data.frame(
      Feature = feature,
      Category = category,
      Gain = gain,
      stringsAsFactors = FALSE
    )
  )
}

# 按类别聚合重要性
category_summary <- category_importance %>%
  group_by(Category) %>%
  summarise(
    Total_Gain = sum(Gain),
    Avg_Gain = mean(Gain),
    Feature_Count = n()
  ) %>%
  arrange(desc(Total_Gain))

cat("主题类别重要性总结:\n")
print(category_summary)

# 绘制类别重要性图
if(require(ggplot2)) {
  cat("绘制类别重要性图...\n")
  p_cat <- ggplot(category_summary, aes(x = reorder(Category, Total_Gain), y = Total_Gain)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = "主题类别对盈利预测的重要性",
      x = "主题类别",
      y = "累计增益"
    ) +
    theme_minimal()
  
  print(p_cat)
  # 保存图表
  ggsave("E:/me/RSM/thesis/results/category_importance_plot.png", p_cat, width = 10, height = 8)
}

cat("分析完成!\n")





#---------------------------------------
# Randomforest
#---------------------------------------

library(randomForest)
library(dplyr)
library(pROC)
library(caret)
library(ggplot2)
library(gridExtra)
library(reshape2)

# 假设已有merged_data数据集
# merged_data <- read_excel("E:/me/RSM/thesis/data/results/merged_clean.xlsx")

# 定义财务变量和主题变量（保持与XGBoost相同的变量定义）
financial_vars <- c(
  "ROA", "ROE", "EBIT_Margin", "Gross_Margin", "CFO_to_Assets", 
  "Accruals", "Current_Ratio", "Quick_Ratio", "Cash_Ratio", 
  "Debt_to_Equity", "Debt_to_Assets", "Long_Term_Debt_to_Assets", 
  "Asset_Turnover", "Inventory_Turnover", "Receivables_Turnover", 
  "Market_Cap", "Book_Value", "Market_to_Book", "Price_to_Earnings", 
  "Dividend_Yield", "CAPEX_to_Assets"
)

# 假设您有140个主题，可以根据实际情况调整
num_topics <- 140
topic_vars <- paste0("Topic_", 1:num_topics)

# 合并所有变量
all_vars <- c(financial_vars, topic_vars)

# 使用基于时间的拆分，同XGBoost实现
train_data <- merged_data[merged_data$fyear %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021), ]
test_data <- merged_data[merged_data$fyear %in% c(2022, 2023), ]

# 确认目标变量名称
target_var <- "eps_increase"

# 输出数据集大小
cat("训练集大小:", nrow(train_data), "行\n")
cat("测试集大小:", nrow(test_data), "行\n")

# 复用之前的数据预处理函数
preprocess_data <- function(data, vars, target) {
  # 检查并处理缺失值
  missing_count <- colSums(is.na(data[, vars]))
  vars_with_missing <- names(missing_count[missing_count > 0])
  
  if(length(vars_with_missing) > 0) {
    cat("处理缺失值的变量:", paste(vars_with_missing, collapse=", "), "\n")
    for(var in vars_with_missing) {
      data[[var]][is.na(data[[var]])] <- median(data[[var]], na.rm=TRUE)
    }
  }
  
  # 检查并处理无限值
  for(var in vars) {
    inf_indices <- is.infinite(data[[var]])
    if(any(inf_indices)) {
      cat(var, "存在无限值，进行处理\n")
      finite_vals <- data[[var]][is.finite(data[[var]])]
      max_val <- max(finite_vals, na.rm=TRUE)
      min_val <- min(finite_vals, na.rm=TRUE)
      data[[var]][data[[var]] == Inf] <- max_val
      data[[var]][data[[var]] == -Inf] <- min_val
    }
  }
  
  # 确保目标变量是因子型
  data[[target]] <- as.factor(data[[target]])
  
  return(data)
}

# 复用F1分数计算函数
calc_f1 <- function(actual, predicted) {
  confusion <- table(Actual = actual, Predicted = predicted)
  
  # 检查混淆矩阵维度，确保为2x2
  if(nrow(confusion) != 2 || ncol(confusion) != 2) {
    # 如果缺少某个类别，手动扩展混淆矩阵
    full_confusion <- matrix(0, nrow=2, ncol=2)
    rownames(full_confusion) <- c("0", "1")
    colnames(full_confusion) <- c("0", "1")
    
    for(i in rownames(confusion)) {
      for(j in colnames(confusion)) {
        full_confusion[i, j] <- confusion[i, j]
      }
    }
    confusion <- full_confusion
  }
  
  tp <- confusion["1", "1"]
  fp <- confusion["0", "1"]
  fn <- confusion["1", "0"]
  
  # 避免除以零
  if(tp == 0) return(0)
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  
  if(precision + recall == 0) return(0)
  f1 <- 2 * precision * recall / (precision + recall)
  
  return(f1)
}

# 评估Random Forest模型的函数
evaluate_rf_model <- function(model, test_data, features, target) {
  # 预测概率
  pred_prob <- predict(model, test_data[, features], type = "prob")[, "1"]
  
  # 转换为二分类预测
  pred_class <- predict(model, test_data[, features], type = "class")
  
  # 实际值
  actual <- test_data[[target]]
  
  # 计算准确率
  accuracy <- mean(pred_class == actual)
  
  # 计算AUC
  roc_obj <- roc(actual, as.numeric(pred_prob), quiet = TRUE)
  auc_val <- auc(roc_obj)
  
  # 计算F1分数
  f1 <- calc_f1(actual, pred_class)
  
  # 计算混淆矩阵
  conf_matrix <- table(Actual = actual, Predicted = pred_class)
  
  # 返回评估结果
  list(
    accuracy = accuracy,
    auc = auc_val,
    f1 = f1,
    conf_matrix = conf_matrix
  )
}

# 预处理训练集和测试集
train_data <- preprocess_data(train_data, all_vars, target_var)
test_data <- preprocess_data(test_data, all_vars, target_var)

#######################################################
# 1. 构建和评估基准模型（仅使用财务变量）
#######################################################

cat("开始基准模型Random Forest训练与调参...\n")

rf_grid <- expand.grid(
  ntree = c(100, 200, 500, 1000),
  mtry = c(2,4,6,8)
)

tuning_results_baseline <- data.frame(
  ntree = numeric(),
  mtry = numeric(),
  accuracy = numeric(),
  auc = numeric(),
  f1 = numeric()
)

# 修改：以AUC为选择标准
best_auc <- 0
best_params_rf_baseline <- NULL

# 生成时间序列交叉验证的foldid
set.seed(123)
foldid_ts <- generate_time_series_foldid(nrow(train_data), n_folds = 5)

for(i in 1:nrow(rf_grid)) {
  ntree_val <- rf_grid$ntree[i]
  mtry_val <- rf_grid$mtry[i]
  
  cat(sprintf("训练Random Forest模型 ntree=%d, mtry=%d\n", ntree_val, mtry_val))
  
  # 使用时间序列交叉验证
  cv_accuracy <- numeric(5)
  cv_auc <- numeric(5)
  cv_f1 <- numeric(5)
  
  for(fold_idx in 1:5) {
    # 根据foldid分割数据
    val_indices <- which(foldid_ts == fold_idx)
    train_fold <- train_data[-val_indices, ]
    val_fold <- train_data[val_indices, ]
    
    # 训练模型
    rf_model <- randomForest(
      x = train_fold[, financial_vars],
      y = train_fold[[target_var]],
      ntree = ntree_val,
      mtry = mtry_val,
      importance = TRUE
    )
    
    # 在验证集上预测
    pred_class <- predict(rf_model, val_fold[, financial_vars])
    pred_prob <- predict(rf_model, val_fold[, financial_vars], type = "prob")[, "1"]
    
    # 计算评估指标
    cv_accuracy[fold_idx] <- mean(pred_class == val_fold[[target_var]])
    cv_auc[fold_idx] <- auc(roc(val_fold[[target_var]], pred_prob, quiet = TRUE))
    cv_f1[fold_idx] <- calc_f1(val_fold[[target_var]], pred_class)
  }
  
  # 计算平均交叉验证评估指标
  mean_accuracy <- mean(cv_accuracy)
  mean_auc <- mean(cv_auc)
  mean_f1 <- mean(cv_f1)
  
  # 将结果添加到调参结果数据框
  tuning_results_baseline <- rbind(tuning_results_baseline, 
                                   data.frame(ntree = ntree_val, 
                                              mtry = mtry_val, 
                                              accuracy = mean_accuracy, 
                                              auc = mean_auc, 
                                              f1 = mean_f1))
  
  cat(sprintf("  时间序列CV平均指标: 准确率=%.4f, AUC=%.4f, F1=%.4f\n", 
              mean_accuracy, mean_auc, mean_f1))
  
  # 修改：如果这组参数达到更好的AUC，则更新最佳参数
  if(mean_auc > best_auc) {
    best_auc <- mean_auc
    best_params_rf_baseline <- list(
      ntree = ntree_val,
      mtry = mtry_val
    )
  }
}

# 展示参数调优结果
cat("基准模型最佳参数 (基于AUC):\n")
print(best_params_rf_baseline)
cat("最佳AUC:", best_auc, "\n")

# 使用最佳参数训练基准模型
baseline_rf_model <- randomForest(
  x = train_data[, financial_vars],
  y = train_data[[target_var]],
  ntree = best_params_rf_baseline$ntree,
  mtry = best_params_rf_baseline$mtry,
  importance = TRUE
)

# 评估基准模型
baseline_rf_eval <- evaluate_rf_model(baseline_rf_model, test_data, financial_vars, target_var)

cat("基准RF模型性能评估结果:\n")
cat("Accuracy:", round(baseline_rf_eval$accuracy, 4), "\n")
cat("AUC:", round(baseline_rf_eval$auc, 4), "\n")
cat("F1 Score:", round(baseline_rf_eval$f1, 4), "\n")
cat("混淆矩阵:\n")
print(baseline_rf_eval$conf_matrix)

#######################################################
# 创建调参可视化 - 基准模型
#######################################################
library(tidyr)

library(ggplot2)
library(patchwork)

# 1. 数据准备（长格式转换）
tuning_baseline_long <- tuning_results_baseline %>%
  pivot_longer(
    cols = c(accuracy, auc, f1),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric, 
                    levels = c("accuracy", "auc", "f1"),
                    labels = c("Accuracy", "AUC", "F1 Score"))
  )

# 2. 热力图函数（蓝色渐变）
plot_baseline_heatmap <- function(data, metric_name, title) {
  ggplot(data %>% filter(metric == metric_name), 
         aes(x = factor(mtry), y = factor(ntree), fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(value, 3)), color = "black", size = 3) +
    scale_fill_gradientn(
      colors = c("#f7fbff", "#6baed6", "#08519c"),  # 蓝白渐变（高对比）
      limits = c(min(tuning_baseline_long$value), max(tuning_baseline_long$value)),
      na.value = "grey90"
    ) +
    labs(
      title = paste(title, "(Baseline Model)"),
      x = "mtry (Number of Features at Split)",
      y = "ntree (Number of Trees)",
      fill = metric_name
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.key.height = unit(1.5, "cm")
    )
}

# 3. 生成三个热力图
baseline_heatmap_acc <- plot_baseline_heatmap(tuning_baseline_long, "Accuracy", "Accuracy")
baseline_heatmap_auc <- plot_baseline_heatmap(tuning_baseline_long, "AUC", "AUC")
baseline_heatmap_f1 <- plot_baseline_heatmap(tuning_baseline_long, "F1 Score", "F1 Score")

# 4. 合并热力图
combined_baseline_heatmaps <- (baseline_heatmap_acc | baseline_heatmap_auc | baseline_heatmap_f1) +
  plot_layout(guides = "collect")

# 5. 保存结果
pdf("Baseline_Model_Tuning_Heatmaps.pdf", width = 16, height = 6)
print(combined_baseline_heatmaps)
dev.off()

# 可选：保存为PNG
ggsave("baseline_heatmaps.png", combined_baseline_heatmaps, width = 14, height = 5, dpi = 300)
print(combined_heatmaps)

best_mtry <- best_params_rf_baseline$mtry
ntree_plot <- tuning_long %>%
  filter(mtry == best_mtry) %>%
  ggplot(aes(x = ntree, y = value, color = metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  labs(
    title = paste("Performance vs. ntree (mtry =", best_mtry, ")"),
    x = "Number of Trees (ntree)",
    y = "Metric Value",
    color = "Metric"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    strip.background = element_rect(fill = "#f0f0f0")
  )

best_ntree <- best_params_rf_baseline$ntree
mtry_plot <- tuning_long %>%
  filter(ntree == best_ntree) %>%
  ggplot(aes(x = mtry, y = value, color = metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  labs(
    title = paste("Performance vs. mtry (ntree =", best_ntree, ")"),
    x = "Number of Features at Split (mtry)",
    y = "Metric Value",
    color = "Metric"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    strip.background = element_rect(fill = "#f0f0f0")
  )
#######################################################
# 2. 构建和评估增强模型（财务变量 + 主题变量）
#######################################################


cat("\n开始增强模型Random Forest训练与调参...\n")

tuning_results_enhanced <- data.frame(
  ntree = numeric(),
  mtry = numeric(),
  accuracy = numeric(),
  auc = numeric(),
  f1 = numeric()
)

# 修改：以AUC为选择标准
best_auc_enhanced <- 0
best_params_rf_enhanced <- NULL

enhanced_rf_grid <- expand.grid(
  ntree = c(100,200,500,1000),
  mtry = c(6,8,10,12,14)
)

for(i in 1:nrow(enhanced_rf_grid)) {
  ntree_val <- enhanced_rf_grid$ntree[i]
  mtry_val <- enhanced_rf_grid$mtry[i]
  
  cat(sprintf("训练增强Random Forest模型 ntree=%d, mtry=%d\n", ntree_val, mtry_val))
  
  # 使用相同的时间序列交叉验证
  cv_accuracy <- numeric(5)
  cv_auc <- numeric(5)
  cv_f1 <- numeric(5)
  
  for(fold_idx in 1:5) {
    val_indices <- which(foldid_ts == fold_idx)
    train_fold <- train_data[-val_indices, ]
    val_fold <- train_data[val_indices, ]
    
    # 训练模型（使用全部变量）
    rf_model <- randomForest(
      x = train_fold[, all_vars],
      y = train_fold[[target_var]],
      ntree = ntree_val,
      mtry = mtry_val,
      importance = TRUE
    )
    
    # 在验证集上预测
    pred_class <- predict(rf_model, val_fold[, all_vars])
    pred_prob <- predict(rf_model, val_fold[, all_vars], type = "prob")[, "1"]
    
    # 计算评估指标
    cv_accuracy[fold_idx] <- mean(pred_class == val_fold[[target_var]])
    cv_auc[fold_idx] <- auc(roc(val_fold[[target_var]], pred_prob, quiet = TRUE))
    cv_f1[fold_idx] <- calc_f1(val_fold[[target_var]], pred_class)
  }
  
  # 计算平均交叉验证评估指标
  mean_accuracy <- mean(cv_accuracy)
  mean_auc <- mean(cv_auc)
  mean_f1 <- mean(cv_f1)
  
  # 将结果添加到调参结果数据框
  tuning_results_enhanced <- rbind(tuning_results_enhanced, 
                                   data.frame(ntree = ntree_val, 
                                              mtry = mtry_val, 
                                              accuracy = mean_accuracy, 
                                              auc = mean_auc, 
                                              f1 = mean_f1))
  
  cat(sprintf("  时间序列CV平均指标: 准确率=%.4f, AUC=%.4f, F1=%.4f\n", 
              mean_accuracy, mean_auc, mean_f1))
  
  # 修改：如果这组参数达到更好的AUC，则更新最佳参数
  if(mean_auc > best_auc_enhanced) {
    best_auc_enhanced <- mean_auc
    best_params_rf_enhanced <- list(
      ntree = ntree_val,
      mtry = mtry_val
    )
  }
}

# 展示参数调优结果
cat("增强模型最佳参数 (基于AUC):\n")
print(best_params_rf_enhanced)
cat("最佳AUC:", best_auc_enhanced, "\n")

# 使用最佳参数训练增强模型
enhanced_rf_model <- randomForest(
  x = train_data[, all_vars],
  y = train_data[[target_var]],
  ntree = best_params_rf_enhanced$ntree,
  mtry = best_params_rf_enhanced$mtry,
  importance = TRUE
)

# 评估增强模型
enhanced_rf_eval <- evaluate_rf_model(enhanced_rf_model, test_data, all_vars, target_var)

cat("增强RF模型性能评估结果:\n")
cat("Accuracy:", round(enhanced_rf_eval$accuracy, 4), "\n")
cat("AUC:", round(enhanced_rf_eval$auc, 4), "\n")
cat("F1 Score:", round(enhanced_rf_eval$f1, 4), "\n")
cat("混淆矩阵:\n")
print(enhanced_rf_eval$conf_matrix)


# Visualization

# 将增强模型的调参结果转为长格式
tuning_enhanced_long <- tuning_results_enhanced %>%
  pivot_longer(
    cols = c(accuracy, auc, f1),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric, 
                    levels = c("accuracy", "auc", "f1"),
                    labels = c("Accuracy", "AUC", "F1 Score"))
  )

plot_enhanced_heatmap <- function(data, metric_name, title) {
  ggplot(data %>% filter(metric == metric_name), 
         aes(x = factor(mtry), y = factor(ntree), fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(value, 3)), color = "black", size = 3) +  # 添加数值标签
    scale_fill_gradientn(
      colors = c("#f7fcf5", "#41ab5d", "#00441b"),  # 绿白渐变（高对比）
      limits = c(min(tuning_enhanced_long$value), max(tuning_enhanced_long$value)),
      na.value = "grey90"
    ) +
    labs(
      title = paste(title, "(Enhanced Model)"),
      x = "mtry (Number of Features at Split)",
      y = "ntree (Number of Trees)",
      fill = metric_name
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.key.height = unit(1.5, "cm")
    )
}

# 生成三个热力图
enhanced_heatmap_acc <- plot_enhanced_heatmap(tuning_enhanced_long, "Accuracy", "Accuracy")
enhanced_heatmap_auc <- plot_enhanced_heatmap(tuning_enhanced_long, "AUC", "AUC")
enhanced_heatmap_f1 <- plot_enhanced_heatmap(tuning_enhanced_long, "F1 Score", "F1 Score")

# 合并热力图
combined_enhanced_heatmaps <- (enhanced_heatmap_acc | enhanced_heatmap_auc | enhanced_heatmap_f1) +
  plot_layout(guides = "collect")

best_mtry_enhanced <- best_params_rf_enhanced$mtry
ntree_plot_enhanced <- tuning_enhanced_long %>%
  filter(mtry == best_mtry_enhanced) %>%
  ggplot(aes(x = ntree, y = value, color = metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, shape = 18) +  # 菱形点
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  labs(
    title = paste("Enhanced Model: Performance vs. ntree (mtry =", best_mtry_enhanced, ")"),
    x = "Number of Trees (ntree)",
    y = "Metric Value"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "none",
    strip.background = element_rect(fill = "#f0f0f0")
  )


best_ntree_enhanced <- best_params_rf_enhanced$ntree
mtry_plot_enhanced <- tuning_enhanced_long %>%
  filter(ntree == best_ntree_enhanced) %>%
  ggplot(aes(x = mtry, y = value, color = metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, shape = 17) +  # 三角形点
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  labs(
    title = paste("Enhanced Model: Performance vs. mtry (ntree =", best_ntree_enhanced, ")"),
    x = "Number of Features at Split (mtry)",
    y = "Metric Value"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "none",
    strip.background = element_rect(fill = "#f0f0f0")
  )

#######################################################
# 3. 模型比较与可视化
#######################################################

# 创建性能比较表
performance_comparison <- data.frame(
  Model = c("Base Random Forest", "Enhanced Random Forest"),
  Accuracy = c(baseline_rf_eval$accuracy, enhanced_rf_eval$accuracy),
  AUC = c(baseline_rf_eval$auc, enhanced_rf_eval$auc),
  F1_Score = c(baseline_rf_eval$f1, enhanced_rf_eval$f1)
)

print(performance_comparison)

# 可视化模型比较
comparison_data <- data.frame(
  Metric = rep(c("Accuracy", "AUC", "F1 Score"), each = 2),
  Model = rep(c("Base","Enhanced"), 3),
  Value = c(baseline_rf_eval$accuracy, enhanced_rf_eval$accuracy,
            baseline_rf_eval$auc, enhanced_rf_eval$auc,
            baseline_rf_eval$f1, enhanced_rf_eval$f1)
)

comparison_plot <- ggplot(comparison_data, aes(x = Metric, y = Value, color = Model, group = Model)) +
  geom_line(size = 1) +  # 绘制折线
  geom_point(size = 3) + # 可选：添加数据点
  labs(title = "Performance Comparison",
       x = "Metrics",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_color_manual(values = c("Base" = "steelblue", "Enhanced" = "darkred")) 

ggsave("模型性能比较.png", comparison_plot, width = 10, height = 6)

# 创建ROC曲线比较图
pdf("ROC曲线比较.pdf", width = 8, height = 6)
baseline_roc <- roc(test_data[[target_var]], 
                    predict(baseline_rf_model, test_data[, financial_vars], type = "prob")[, "1"],
                    quiet = TRUE)

enhanced_roc <- roc(test_data[[target_var]], 
                    predict(enhanced_rf_model, test_data[, all_vars], type = "prob")[, "1"],
                    quiet = TRUE)

plot(baseline_roc, col = "blue", main = "ROC comparison", lwd = 2)
lines(enhanced_roc, col = "red", lwd = 2)
legend("bottomright", legend = c(
  paste("Base (AUC =", round(baseline_rf_eval$auc, 3), ")"),
  paste("Enhance(AUC =", round(enhanced_rf_eval$auc, 3), ")")
), col = c("blue", "red"), lwd = 2)
dev.off()



#---------------------------------------
# Lasso Regression Analysis
#---------------------------------------

library(glmnet)
library(dplyr)
library(pROC)
library(caret)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(tidyr)
library(patchwork)

# 定义财务变量和主题变量（与之前保持一致）
financial_vars <- c(
  "ROA", "ROE", "EBIT_Margin", "Gross_Margin", "CFO_to_Assets", 
  "Accruals", "Current_Ratio", "Quick_Ratio", "Cash_Ratio", 
  "Debt_to_Equity", "Debt_to_Assets", "Long_Term_Debt_to_Assets", 
  "Asset_Turnover", "Inventory_Turnover", "Receivables_Turnover", 
  "Market_Cap", "Book_Value", "Market_to_Book", "Price_to_Earnings", 
  "Dividend_Yield", "CAPEX_to_Assets"
)

# 假设您有140个主题，可以根据实际情况调整
num_topics <- 100
topic_vars <- paste0("Topic_", 1:num_topics)

# 合并所有变量
all_vars <- c(financial_vars, topic_vars)

# 使用基于时间的拆分
train_data <- merged_data[merged_data$fyear %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021), ]
test_data <- merged_data[merged_data$fyear %in% c(2022, 2023), ]

# 确认目标变量名称
target_var <- "eps_increase"

# 输出数据集大小
cat("训练集大小:", nrow(train_data), "行\n")
cat("测试集大小:", nrow(test_data), "行\n")

# 预处理数据（复用之前的函数）
train_data <- preprocess_data(train_data, all_vars, target_var)
test_data <- preprocess_data(test_data, all_vars, target_var)

# 标准化数据（Lasso对特征尺度敏感）
standardize_for_lasso <- function(train_data, test_data, vars) {
  # 计算训练集的均值和标准差
  train_means <- sapply(train_data[, vars], mean, na.rm = TRUE)
  train_sds <- sapply(train_data[, vars], sd, na.rm = TRUE)
  
  # 标准化训练集
  train_scaled <- train_data
  for(var in vars) {
    if(train_sds[var] > 0) {
      train_scaled[[var]] <- (train_data[[var]] - train_means[var]) / train_sds[var]
    }
  }
  
  # 使用训练集参数标准化测试集
  test_scaled <- test_data
  for(var in vars) {
    if(train_sds[var] > 0) {
      test_scaled[[var]] <- (test_data[[var]] - train_means[var]) / train_sds[var]
    }
  }
  
  return(list(
    train = train_scaled, 
    test = test_scaled, 
    means = train_means, 
    sds = train_sds
  ))
}

# Lasso模型评估函数
evaluate_lasso_model <- function(model, test_x, test_y, lambda) {
  # 预测概率
  pred_prob <- predict(model, newx = test_x, s = lambda, type = "response")[, 1]
  
  # 转换为二分类预测
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  # 实际值（转换为数值）
  actual <- as.numeric(test_y) - 1
  
  # 计算准确率
  accuracy <- mean(pred_class == actual)
  
  # 计算AUC
  roc_obj <- roc(actual, pred_prob, quiet = TRUE)
  auc_val <- auc(roc_obj)
  
  # 计算F1分数
  f1 <- calc_f1(actual, pred_class)
  
  # 计算混淆矩阵
  conf_matrix <- table(Actual = actual, Predicted = pred_class)
  
  return(list(
    accuracy = accuracy,
    auc = auc_val,
    f1 = f1,
    conf_matrix = conf_matrix,
    pred_prob = pred_prob,
    pred_class = pred_class
  ))
}

#######################################################
# 1. 构建和评估基准模型（仅使用财务变量）
#######################################################

cat("开始基准模型Lasso回归训练...\n")

# 标准化财务变量
scaled_data_baseline <- standardize_for_lasso(train_data, test_data, financial_vars)
train_scaled_baseline <- scaled_data_baseline$train
test_scaled_baseline <- scaled_data_baseline$test

# 准备Lasso数据
X_train_baseline <- as.matrix(train_scaled_baseline[, financial_vars])
y_train_baseline <- as.numeric(train_scaled_baseline[[target_var]]) - 1  # 转换为0,1

X_test_baseline <- as.matrix(test_scaled_baseline[, financial_vars])
y_test_baseline <- as.numeric(test_scaled_baseline[[target_var]]) - 1

# 使用交叉验证找到最佳lambda
set.seed(123)
cv_lasso_baseline <- cv.glmnet(
  x = X_train_baseline,
  y = y_train_baseline,
  family = "binomial",
  alpha = 1,  # alpha=1表示Lasso
  nfolds = 5,
  type.measure = "auc",  # 使用AUC作为评估指标
  standardize = FALSE  # 已经手动标准化了
)

# 绘制交叉验证结果
pdf("Lasso_基准模型_交叉验证.pdf", width = 8, height = 6)
plot(cv_lasso_baseline, main = "Lasso Regression CV (Baseline Model)")
dev.off()

# 获取最佳lambda
lambda_min_baseline <- cv_lasso_baseline$lambda.min
lambda_1se_baseline <- cv_lasso_baseline$lambda.1se

cat("基准模型最佳lambda (min):", lambda_min_baseline, "\n")
cat("基准模型最佳lambda (1se):", lambda_1se_baseline, "\n")

# 使用lambda.1se训练最终模型（更保守，泛化能力更好）
lasso_baseline_model <- glmnet(
  x = X_train_baseline,
  y = y_train_baseline,
  family = "binomial",
  alpha = 1,
  lambda = lambda_1se_baseline,
  standardize = FALSE
)

# 评估基准模型
baseline_lasso_eval <- evaluate_lasso_model(
  lasso_baseline_model, X_test_baseline, test_scaled_baseline[[target_var]], lambda_1se_baseline
)

cat("基准Lasso模型性能评估结果:\n")
cat("Accuracy:", round(baseline_lasso_eval$accuracy, 4), "\n")
cat("AUC:", round(baseline_lasso_eval$auc, 4), "\n")
cat("F1 Score:", round(baseline_lasso_eval$f1, 4), "\n")
cat("混淆矩阵:\n")
print(baseline_lasso_eval$conf_matrix)

# 分析基准模型的特征选择
baseline_coefs <- coef(lasso_baseline_model, s = lambda_1se_baseline)
baseline_coef_df <- data.frame(
  Feature = rownames(baseline_coefs)[-1],  # 排除截距
  Coefficient = as.numeric(baseline_coefs[-1, 1]),
  stringsAsFactors = FALSE
)

# 筛选非零系数
baseline_nonzero <- baseline_coef_df[baseline_coef_df$Coefficient != 0, ]
baseline_nonzero <- baseline_nonzero[order(abs(baseline_nonzero$Coefficient), decreasing = TRUE), ]

cat("\n基准模型选择的特征数量:", nrow(baseline_nonzero), "/", length(financial_vars), "\n")
cat("选择的特征:\n")
print(baseline_nonzero)

#######################################################
# 1. 构建和评估基准模型（仅使用财务变量）
# 使用时间序列风格的交叉验证（非随机K折）
#######################################################

cat("开始基准模型Lasso回归训练...\n")

# 辅助函数：基于时间顺序生成foldid
generate_time_series_foldid <- function(n_obs, n_folds = 5) {
  fold_sizes <- floor(n_obs / n_folds)
  remainder <- n_obs %% n_folds
  
  foldid <- rep(NA, n_obs)
  start <- 1
  
  for (i in 1:n_folds) {
    size <- fold_sizes + ifelse(i <= remainder, 1, 0)
    end <- start + size - 1
    foldid[start:end] <- i
    start <- end + 1
  }
  
  return(foldid)
}

# 标准化财务变量
scaled_data_baseline <- standardize_for_lasso(train_data, test_data, financial_vars)
train_scaled_baseline <- scaled_data_baseline$train
test_scaled_baseline <- scaled_data_baseline$test

# 准备Lasso数据
X_train_baseline <- as.matrix(train_scaled_baseline[, financial_vars])
y_train_baseline <- as.numeric(train_scaled_baseline[[target_var]]) - 1

X_test_baseline <- as.matrix(test_scaled_baseline[, financial_vars])
y_test_baseline <- as.numeric(test_scaled_baseline[[target_var]]) - 1

# 使用时间顺序foldid代替默认随机5折
set.seed(123)
foldid_ts <- generate_time_series_foldid(nrow(X_train_baseline), n_folds = 5)

cv_lasso_baseline <- cv.glmnet(
  x = X_train_baseline,
  y = y_train_baseline,
  family = "binomial",
  alpha = 1,
  foldid = foldid_ts,
  type.measure = "auc",
  standardize = FALSE
)

# 绘制交叉验证结果
pdf("Lasso_基准模型_交叉验证_时间序列CV.pdf", width = 8, height = 6)
plot(cv_lasso_baseline, main = "Lasso Regression CV (Baseline Model - Time Series)")
dev.off()

# 获取最佳lambda
lambda_min_baseline <- cv_lasso_baseline$lambda.min
lambda_1se_baseline <- cv_lasso_baseline$lambda.1se

cat("基准模型最佳lambda (min):", lambda_min_baseline, "\n")
cat("基准模型最佳lambda (1se):", lambda_1se_baseline, "\n")

# 使用lambda.1se训练最终模型
lasso_baseline_model <- glmnet(
  x = X_train_baseline,
  y = y_train_baseline,
  family = "binomial",
  alpha = 1,
  lambda = lambda_1se_baseline,
  standardize = FALSE
)

# 评估基准模型
baseline_lasso_eval <- evaluate_lasso_model(
  lasso_baseline_model, X_test_baseline, test_scaled_baseline[[target_var]], lambda_1se_baseline
)

cat("基准Lasso模型性能评估结果:\n")
cat("Accuracy:", round(baseline_lasso_eval$accuracy, 4), "\n")
cat("AUC:", round(baseline_lasso_eval$auc, 4), "\n")
cat("F1 Score:", round(baseline_lasso_eval$f1, 4), "\n")
cat("混淆矩阵:\n")
print(baseline_lasso_eval$conf_matrix)

# 分析特征选择结果
baseline_coefs <- coef(lasso_baseline_model, s = lambda_1se_baseline)
baseline_coef_df <- data.frame(
  Feature = rownames(baseline_coefs)[-1],
  Coefficient = as.numeric(baseline_coefs[-1, 1]),
  stringsAsFactors = FALSE
)

baseline_nonzero <- baseline_coef_df[baseline_coef_df$Coefficient != 0, ]
baseline_nonzero <- baseline_nonzero[order(abs(baseline_nonzero$Coefficient), decreasing = TRUE), ]

cat("\n基准模型选择的特征数量:", nrow(baseline_nonzero), "/", length(financial_vars), "\n")
cat("选择的特征:\n")
print(baseline_nonzero)


#######################################################
# 2. 构建和评估增强模型（财务变量 + 主题变量）
#######################################################

cat("\n开始增强模型Lasso回归训练...\n")

# 标准化所有变量
scaled_data_enhanced <- standardize_for_lasso(train_data, test_data, all_vars)
train_scaled_enhanced <- scaled_data_enhanced$train
test_scaled_enhanced <- scaled_data_enhanced$test

# 准备Lasso数据
X_train_enhanced <- as.matrix(train_scaled_enhanced[, all_vars])
y_train_enhanced <- as.numeric(train_scaled_enhanced[[target_var]]) - 1

X_test_enhanced <- as.matrix(test_scaled_enhanced[, all_vars])
y_test_enhanced <- as.numeric(test_scaled_enhanced[[target_var]]) - 1

# 使用交叉验证找到最佳lambda（可能需要更大的lambda范围）
set.seed(123)
cv_lasso_enhanced <- cv.glmnet(
  x = X_train_enhanced,
  y = y_train_enhanced,
  family = "binomial",
  alpha = 1,
  nfolds = 5,
  type.measure = "auc",
  standardize = FALSE,
  # 为高维数据设置更大的lambda范围
  lambda = exp(seq(log(0.001), log(1), length.out = 100))
)

# 绘制交叉验证结果
pdf("Lasso_增强模型_交叉验证.pdf", width = 8, height = 6)
plot(cv_lasso_enhanced, main = "Lasso Regression CV (Enhanced Model)")
dev.off()

# 获取最佳lambda
lambda_min_enhanced <- cv_lasso_enhanced$lambda.min
lambda_1se_enhanced <- cv_lasso_enhanced$lambda.1se

cat("增强模型最佳lambda (min):", lambda_min_enhanced, "\n")
cat("增强模型最佳lambda (1se):", lambda_1se_enhanced, "\n")

# 使用lambda.1se训练最终模型
lasso_enhanced_model <- glmnet(
  x = X_train_enhanced,
  y = y_train_enhanced,
  family = "binomial",
  alpha = 1,
  lambda = lambda_1se_enhanced,
  standardize = FALSE
)

# 评估增强模型
enhanced_lasso_eval <- evaluate_lasso_model(
  lasso_enhanced_model, X_test_enhanced, test_scaled_enhanced[[target_var]], lambda_1se_enhanced
)

cat("增强Lasso模型性能评估结果:\n")
cat("Accuracy:", round(enhanced_lasso_eval$accuracy, 4), "\n")
cat("AUC:", round(enhanced_lasso_eval$auc, 4), "\n")
cat("F1 Score:", round(enhanced_lasso_eval$f1, 4), "\n")
cat("混淆矩阵:\n")
print(enhanced_lasso_eval$conf_matrix)


# 提取 lambda 和对应的 mean AUC 以及标准误
cv_data <- data.frame(
  lambda = cv_lasso_baseline$lambda,
  auc = cv_lasso_baseline$cvm,             # mean AUC across folds
  se = cv_lasso_baseline$cvsd              # standard error of AUC
)

# 添加最佳 lambda 信息
lambda_min <- cv_lasso_baseline$lambda.min
lambda_1se <- cv_lasso_baseline$lambda.1se

# 创建图形
ggplot(cv_data, aes(x = log(lambda), y = auc)) +
  geom_line(color = "#2C3E50", size = 1) +
  geom_ribbon(aes(ymin = auc - se, ymax = auc + se), alpha = 0.2, fill = "#2980B9") +
  geom_vline(xintercept = log(lambda_min), linetype = "dashed", color = "red") +
  geom_vline(xintercept = log(lambda_1se), linetype = "dotted", color = "blue") +
  labs(
    title = "Lasso CV Tuning - AUC vs log(Lambda)",
    subtitle = "红线：lambda.min，蓝线：lambda.1se",
    x = "log(Lambda)",
    y = "Mean AUC (5-fold CV)"
  ) +
  theme_minimal()

#######################################################
# 2. 构建和评估增强模型（财务变量 + 主题变量）
# 使用时间序列风格交叉验证（非随机K折）
#######################################################

cat("\n开始增强模型Lasso回归训练...\n")

# 标准化所有变量
scaled_data_enhanced <- standardize_for_lasso(train_data, test_data, all_vars)
train_scaled_enhanced <- scaled_data_enhanced$train
test_scaled_enhanced <- scaled_data_enhanced$test

# 准备Lasso数据
X_train_enhanced <- as.matrix(train_scaled_enhanced[, all_vars])
y_train_enhanced <- as.numeric(train_scaled_enhanced[[target_var]]) - 1

X_test_enhanced <- as.matrix(test_scaled_enhanced[, all_vars])
y_test_enhanced <- as.numeric(test_scaled_enhanced[[target_var]]) - 1

# 构造时间顺序foldid
set.seed(123)
foldid_ts_enhanced <- generate_time_series_foldid(nrow(X_train_enhanced), n_folds = 5)

# 使用交叉验证找到最佳lambda（支持高维稀疏数据的lambda范围）
cv_lasso_enhanced <- cv.glmnet(
  x = X_train_enhanced,
  y = y_train_enhanced,
  family = "binomial",
  alpha = 1,
  foldid = foldid_ts_enhanced,
  type.measure = "auc",
  standardize = FALSE,
  lambda = exp(seq(log(0.001), log(1), length.out = 100))
)

# 绘制交叉验证结果
pdf("Lasso_增强模型_交叉验证_时间序列CV.pdf", width = 8, height = 6)
plot(cv_lasso_enhanced, main = "Lasso Regression CV (Enhanced Model - Time Series)")
dev.off()

# 获取最佳lambda
lambda_min_enhanced <- cv_lasso_enhanced$lambda.min
lambda_1se_enhanced <- cv_lasso_enhanced$lambda.1se

cat("增强模型最佳lambda (min):", lambda_min_enhanced, "\n")
cat("增强模型最佳lambda (1se):", lambda_1se_enhanced, "\n")

# 使用lambda.1se训练最终模型
lasso_enhanced_model <- glmnet(
  x = X_train_enhanced,
  y = y_train_enhanced,
  family = "binomial",
  alpha = 1,
  lambda = lambda_1se_enhanced,
  standardize = FALSE
)

# 评估增强模型
enhanced_lasso_eval <- evaluate_lasso_model(
  lasso_enhanced_model, X_test_enhanced, test_scaled_enhanced[[target_var]], lambda_1se_enhanced
)

cat("增强Lasso模型性能评估结果:\n")
cat("Accuracy:", round(enhanced_lasso_eval$accuracy, 4), "\n")
cat("AUC:", round(enhanced_lasso_eval$auc, 4), "\n")
cat("F1 Score:", round(enhanced_lasso_eval$f1, 4), "\n")
cat("混淆矩阵:\n")
print(enhanced_lasso_eval$conf_matrix)



#######################################################
# 3. 特征选择分析
#######################################################

cat("\n分析增强模型的特征选择结果...\n")

# 提取增强模型的系数
enhanced_coefs <- coef(lasso_enhanced_model, s = lambda_1se_enhanced)
enhanced_coef_df <- data.frame(
  Feature = rownames(enhanced_coefs)[-1],  # 排除截距
  Coefficient = as.numeric(enhanced_coefs[-1, 1]),
  stringsAsFactors = FALSE
)

# 添加变量类型标签
enhanced_coef_df$Type <- ifelse(enhanced_coef_df$Feature %in% financial_vars, "Financial", "Topic")

# 筛选非零系数
enhanced_nonzero <- enhanced_coef_df[enhanced_coef_df$Coefficient != 0, ]
enhanced_nonzero <- enhanced_nonzero[order(abs(enhanced_nonzero$Coefficient), decreasing = TRUE), ]

cat("增强模型选择的特征数量:", nrow(enhanced_nonzero), "/", length(all_vars), "\n")

# 按类型统计
feature_selection_summary <- enhanced_nonzero %>%
  group_by(Type) %>%
  summarize(
    Count = n(),
    Avg_Abs_Coef = mean(abs(Coefficient)),
    Max_Abs_Coef = max(abs(Coefficient)),
    .groups = "drop"
  )

print(feature_selection_summary)

# 显示选择的财务变量
selected_financial <- enhanced_nonzero[enhanced_nonzero$Type == "Financial", ]
cat("\n选择的财务变量 (", nrow(selected_financial), "/", length(financial_vars), "):\n")
print(selected_financial)

# 显示选择的主题变量
selected_topics <- enhanced_nonzero[enhanced_nonzero$Type == "Topic", ]
cat("\n选择的主题变量 (", nrow(selected_topics), "/", length(topic_vars), "):\n")
print(head(selected_topics, 15))  # 显示前15个

#######################################################
# 4. 可视化分析
#######################################################

# 4.1 模型性能比较
performance_comparison <- data.frame(
  Model = c("Baseline Lasso", "Enhanced Lasso"),
  Accuracy = c(baseline_lasso_eval$accuracy, enhanced_lasso_eval$accuracy),
  AUC = c(baseline_lasso_eval$auc, enhanced_lasso_eval$auc),
  F1_Score = c(baseline_lasso_eval$f1, enhanced_lasso_eval$f1)
)

print(performance_comparison)

# 计算性能提升
improvement <- list(
  accuracy = (enhanced_lasso_eval$accuracy - baseline_lasso_eval$accuracy) / baseline_lasso_eval$accuracy * 100,
  auc = (enhanced_lasso_eval$auc - baseline_lasso_eval$auc) / baseline_lasso_eval$auc * 100,
  f1 = (enhanced_lasso_eval$f1 - baseline_lasso_eval$f1) / baseline_lasso_eval$f1 * 100
)

cat("\n模型性能提升比较:\n")
cat("Accuracy提升:", round(improvement$accuracy, 2), "%\n")
cat("AUC提升:", round(improvement$auc, 2), "%\n")
cat("F1 Score提升:", round(improvement$f1, 2), "%\n")

# 4.2 性能比较图
comparison_data <- data.frame(
  Metric = rep(c("Accuracy", "AUC", "F1 Score"), each = 2),
  Model = rep(c("Baseline", "Enhanced"), 3),
  Value = c(baseline_lasso_eval$accuracy, enhanced_lasso_eval$accuracy,
            baseline_lasso_eval$auc, enhanced_lasso_eval$auc,
            baseline_lasso_eval$f1, enhanced_lasso_eval$f1)
)

comparison_plot <- ggplot(comparison_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Value, 3)), position = position_dodge(width = 0.7), vjust = -0.5) +
  scale_fill_manual(values = c("Baseline" = "steelblue", "Enhanced" = "darkgreen")) +
  labs(title = "Lasso Regression Model Performance Comparison",
       subtitle = "Financial Variables vs. Financial + Topic Variables",
       x = "Metrics", y = "Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  ylim(0, max(comparison_data$Value) * 1.1)

print(comparison_plot)
ggsave("Lasso_模型性能比较.png", comparison_plot, width = 10, height = 6, dpi = 300)

# 4.3 特征重要性可视化（Top 20）
top_features <- head(enhanced_nonzero, 20)

feature_importance_plot <- ggplot(top_features, aes(x = reorder(Feature, abs(Coefficient)), 
                                                    y = abs(Coefficient), fill = Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Financial" = "steelblue", "Topic" = "darkgreen")) +
  labs(title = "Top 20 Features Selected by Lasso",
       subtitle = paste("Selected", nrow(enhanced_nonzero), "out of", length(all_vars), "features"),
       x = "Features", y = "Absolute Coefficient Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

print(feature_importance_plot)
ggsave("Lasso_特征重要性.png", feature_importance_plot, width = 10, height = 8, dpi = 300)

# 4.4 系数路径图
pdf("Lasso_系数路径_增强模型.pdf", width = 10, height = 6)
plot(cv_lasso_enhanced$glmnet.fit, xvar = "lambda", main = "Lasso Coefficient Path (Enhanced Model)")
abline(v = log(lambda_1se_enhanced), col = "red", lty = 2)
legend("topright", legend = "Selected λ", col = "red", lty = 2)
dev.off()

# 4.5 ROC曲线比较
pdf("Lasso_ROC曲线比较.pdf", width = 8, height = 6)

baseline_roc <- roc(y_test_baseline, baseline_lasso_eval$pred_prob, quiet = TRUE)
enhanced_roc <- roc(y_test_enhanced, enhanced_lasso_eval$pred_prob, quiet = TRUE)

plot(baseline_roc, col = "blue", main = "Lasso ROC Curve Comparison", lwd = 2)
lines(enhanced_roc, col = "darkgreen", lwd = 2)
legend("bottomright", legend = c(
  paste("Baseline Lasso (AUC =", round(baseline_lasso_eval$auc, 3), ")"),
  paste("Enhanced Lasso (AUC =", round(enhanced_lasso_eval$auc, 3), ")")
), col = c("blue", "darkgreen"), lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")

dev.off()

#######################################################
# 5. 保存结果
#######################################################

# 整合所有结果
lasso_results <- list(
  baseline = list(
    model = lasso_baseline_model,
    cv_result = cv_lasso_baseline,
    lambda = lambda_1se_baseline,
    eval = baseline_lasso_eval,
    selected_features = baseline_nonzero
  ),
  enhanced = list(
    model = lasso_enhanced_model,
    cv_result = cv_lasso_enhanced,
    lambda = lambda_1se_enhanced,
    eval = enhanced_lasso_eval,
    selected_features = enhanced_nonzero,
    feature_summary = feature_selection_summary
  ),
  improvement = improvement,
  performance_comparison = performance_comparison,
  selected_topics = selected_topics
)

# 保存结果
saveRDS(lasso_results, "Lasso_分析结果.rds")
write.csv(enhanced_nonzero, "Lasso_选择的特征.csv", row.names = FALSE)
write.csv(selected_topics, "Lasso_选择的主题变量.csv", row.names = FALSE)
write.csv(performance_comparison, "Lasso_性能比较.csv", row.names = FALSE)

cat("\n所有Lasso分析结果已保存完成！\n")

#######################################################
# 6. 结果总结
#######################################################

cat("\n========= Lasso分析结果总结 =========\n")
cat("基准模型(仅财务变量):\n")
cat("  选择特征数:", nrow(baseline_nonzero), "/", length(financial_vars), "\n")
cat("  准确率:", round(baseline_lasso_eval$accuracy, 4), "\n")
cat("  AUC:", round(baseline_lasso_eval$auc, 4), "\n")
cat("  F1分数:", round(baseline_lasso_eval$f1, 4), "\n")

cat("\n增强模型(财务+主题变量):\n")
cat("  总选择特征数:", nrow(enhanced_nonzero), "/", length(all_vars), "\n")
cat("  选择的财务变量:", nrow(selected_financial), "/", length(financial_vars), "\n")
cat("  选择的主题变量:", nrow(selected_topics), "/", length(topic_vars), "\n")
cat("  准确率:", round(enhanced_lasso_eval$accuracy, 4), "\n")
cat("  AUC:", round(enhanced_lasso_eval$auc, 4), "\n")
cat("  F1分数:", round(enhanced_lasso_eval$f1, 4), "\n")

cat("\n性能提升:\n")
cat("  准确率提升:", round(improvement$accuracy, 2), "%\n")
cat("  AUC提升:", round(improvement$auc, 2), "%\n")
cat("  F1分数提升:", round(improvement$f1, 2), "%\n")

cat("\n主要发现:\n")
if(nrow(selected_topics) > 0) {
  cat("  ✓ Lasso自动选择了", nrow(selected_topics), "个有预测价值的主题变量\n")
  cat("  ✓ 主题变量确实提供了增量预测能力\n")
} else {
  cat("  ✗ Lasso没有选择任何主题变量\n")
  cat("  ✗ 主题变量可能没有显著的预测价值\n")
}

cat("\n分析完成！所有图表和结果文件已保存。\n")