# R script for gbm model to predict loan category (good or bad)
# The feature set does not include risk_score
# validation auc = 

library(h2o)
h2o.init(nthreads = -1)

lc_data <- h2o.importFolder("LendingClub")



lc_data$risk_score <- ifelse(lc_data$last_fico_range_low == 0, NA, 
                             (lc_data$last_fico_range_high + lc_data$last_fico_range_low)/2)


lc_data$emp_length <- h2o.sub(x = lc_data$emp_length, pattern = "([ ]*+[a-zA-Z].*)|(n/a)", replacement = "")
lc_data$emp_length <- h2o.trim(lc_data$emp_length)
lc_data$emp_length <- h2o.sub(x = lc_data$emp_length, pattern = "< 1", replacement = "0")
lc_data$emp_length <- h2o.sub(x = lc_data$emp_length, pattern = "10\\+", replacement = "10")
lc_data$emp_length <- as.h2o(as.numeric(as.matrix(lc_data$emp_length)))


lc_data <- lc_data[!(lc_data$loan_status %in% c("Current", "In Grace Period", 
                                                "Does not meet the credit policy.  Status:Current", 
                                                "Does not meet the credit policy.  Status:In Grace Period")), ]
lc_data <- lc_data[!is.na(lc_data$id),]
lc_data$bad_loan <- lc_data$loan_status %in% c("Charged Off", "Default", 
                                               "Does not meet the credit policy.  Status:Charged Off", 
                                               "Late (16-30 days)", "Late (31-120 days)")
lc_data$bad_loan <- as.factor(lc_data$bad_loan)



data <- lc_data

splits <- h2o.splitFrame(
  data = data,
  ratios = c(0.6,0.2),
  destination_frames = c("train.hex", "valid.hex", "test.hex"),
  seed = 1234
)

train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]



myX <- c("loan_amnt", "term", "emp_length", "annual_inc", "zip_code", "dti", "delinq_2yrs", 
         "purpose", "verification_status", "installment", "home_ownership", "pub_rec", "open_acc", "inq_last_6mths")
myY <- "bad_loan"

gbm_model <- h2o.gbm(x = myX, 
                     y = myY, 
                     training_frame = train, 
                     validation_frame = valid, 
                     model_id = "lc_loanCategory_predictor",
                     balance_classes = T,
                     learn_rate = 0.01, 
                     learn_rate_annealing = 0.99,
                     distribution = "bernoulli",
                     score_each_iteration = T, 
                     ntrees = 1000, 
                     stopping_rounds = 5,
                     stopping_tolerance = 1e-4,
                     stopping_metric = "AUC",
                     sample_rate = 0.5,
                     col_sample_rate = 0.7,
                     col_sample_rate_per_tree = 0.8,
                     max_depth = 12,
                     histogram_type = "QuantilesGlobal",
                     score_tree_interval = 10)

print(gbm_model)

# auc for training data = 
# auc for validation data = 

h2o.download_pojo(gbm_model, getwd())

