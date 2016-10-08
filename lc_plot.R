library(ggplot2)
library(plyr)
library(Hmisc)

# import data
lc_data1 <- read.csv("Documents/LendingClub/LoanStats3a_securev1.csv", header = TRUE, na.strings = "")
lc_data2 <- read.csv("Documents/LendingClub/LoanStats3b_securev1.csv", header = TRUE, na.strings = "")
lc_data3 <- read.csv("Documents/LendingClub/LoanStats3c_securev1.csv", header = TRUE, na.strings = "")
lc_data4 <- read.csv("Documents/LendingClub/LoanStats3d_securev1.csv", header = TRUE, na.strings = "")
lc_data <- rbind(lc_data1, lc_data2, lc_data3, lc_data4)

# Remove rows with NA values in member_id field
lc_data <- lc_data[!(is.na(lc_data$member_id)), ]

# Convert loan_amnt,last_fico_range_low and last_fico_range_high to numeric and create risk_score predictor
lc_data$loan_amnt <- as.numeric(lc_data$loan_amnt)
lc_data$last_fico_range_low <- as.numeric(lc_data$last_fico_range_low)
lc_data$last_fico_range_high <- as.numeric(lc_data$last_fico_range_high)
lc_data$risk_score <- ifelse(lc_data$last_fico_range_low == 0, NA, 
                             (lc_data$last_fico_range_high + lc_data$last_fico_range_low)/2)

# Remove ongoing loans
lc_data <- lc_data[!(lc_data$loan_status %in% c("Current", "In Grace Period", 
                                                "Does not meet the credit policy.  Status:Current", 
                                                "Does not meet the credit policy.  Status:In Grace Period")), ]
lc_data <- lc_data[!is.na(lc_data$id),]
# Create bad loan labels
lc_data$bad_loan <- lc_data$loan_status %in% c("Charged Off", "Default", 
                                               "Does not meet the credit policy.  Status:Charged Off", 
                                               "Late (16-30 days)", "Late (31-120 days)")
lc_data$bad_loan <- as.factor(lc_data$bad_loan)

lc_plotData <- lc_data[ , c("loan_amnt", "term", "risk_score", "bad_loan")]

# Divide data into risk classes (500-550,550-600,600-650,650-700,700-750,750-800,800-850)
lc_plotData$risk_class <- as.numeric(cut2(lc_plotData$risk_score, c(550,600,650,700,750,800)))

# Create loan classes
loan_amnt_cuts <- c(2500,4500,6500,8500,10500,12500,
                14500,16500,18500,20500,22500,24500,
                26500,28500,30500,32500,34500)
lc_plotData$loanAmnt_class <- as.numeric(cut2(lc_plotData$loan_amnt, loan_amnt_cuts))

# Aggregate the number of loans by the loan amount class, risk class and term
totalPerLoanTermRisk <- count(lc_plotData, c("loanAmnt_class", "term", "risk_class"))
totalPerLoanTermRisk <- totalPerLoanTermRisk[!(is.na(totalPerLoanTermRisk$risk_class)), ]
colnames_total_loans <- c("loanAmnt_class", "term", "risk_class", "totalFreq")
colnames(totalPerLoanTermRisk) <- colnames_total_loans

# Aggregate the number of loans by the loan amount class, risk class, term and type of loan(good or bad)
totalPerLoanTermRiskClass <- count(lc_plotData, c("loanAmnt_class", "term", "risk_class", "bad_loan"))
totalPerLoanTermRiskClass <- totalPerLoanTermRiskClass[!(is.na(totalPerLoanTermRiskClass$risk_class)), ]
# Filter out the rows that have loan category as TRUE (we only want the data with good loans)
good_loans <- totalPerLoanTermRiskClass[totalPerLoanTermRiskClass$bad_loan == "FALSE", ]
cols <- c("loanAmnt_class", "term", "risk_class", "freq")
good_loans <- good_loans[ , cols]
colnames_good_loans <- c("loanAmnt_class", "term", "risk_class","goodLoanFreq")
colnames(good_loans) <- colnames_good_loans

# Merge both the frames created above
final_data <- merge(totalPerLoanTermRisk, good_loans, by.x = c("loanAmnt_class", "term", "risk_class"), 
                    by.y = c("loanAmnt_class", "term", "risk_class"), all = TRUE, sort = FALSE)

# Remove NAs
final_data <- final_data[!is.na(final_data$goodLoanFreq), ]

# Calculate the probability of good loans for a particular loan amount class, risk class and term
final_data$goodLoanProb <- final_data$goodLoanFreq/final_data$totalFreq

ggplot(final_data[final_data$risk_class == 1,], aes(x = loanAmnt_class, y = goodLoanProb, colour = term)) + geom_point() + geom_line() + coord_cartesian(ylim = c(0,1))

ggplot(final_data[final_data$risk_class == 2,], aes(x = loanAmnt_class, y = goodLoanProb, colour = term)) + geom_point() + geom_line() + coord_cartesian(ylim = c(0,1))

ggplot(final_data[final_data$risk_class == 3,], aes(x = loanAmnt_class, y = goodLoanProb, colour = term)) + geom_point() + geom_line() + coord_cartesian(ylim = c(0,1))

ggplot(final_data[final_data$risk_class == 4,], aes(x = loanAmnt_class, y = goodLoanProb, colour = term)) + geom_point() + geom_line() + coord_cartesian(ylim = c(0,1))

ggplot(final_data[final_data$risk_class == 5,], aes(x = loanAmnt_class, y = goodLoanProb, colour = term)) + geom_point() + geom_line() + coord_cartesian(ylim = c(0,1))

ggplot(final_data[final_data$risk_class == 6,], aes(x = loanAmnt_class, y = goodLoanProb, colour = term)) + geom_point() + geom_line() + coord_cartesian(ylim = c(0,1))

ggplot(final_data[final_data$risk_class == 7,], aes(x = loanAmnt_class, y = goodLoanProb, colour = term)) + geom_point() + geom_line() + coord_cartesian(ylim = c(0,1))
