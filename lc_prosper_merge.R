rm(list=ls())
library(zoo)

path = "E:/loan_stacking"
setwd(path)
trim <- function (x) {
  y = gsub("^\\s+|\\s+$", "", x)
  y = gsub("[<]","",y)
  y = gsub("[+]","",y)
  return(y)
}

lc_raw = readRDS(file="E:/loan_stacking/lendingclub_raw.rds")
lc_keep = c("id","member_id","loan_amnt","funded_amnt","term","int_rate","emp_length",
            "home_ownership","annual_inc","is_inc_v","accept_d","issue_d","addr_city","addr_state",
            "dti","last_pymnt_d","len","fico","inq_last_6mths","delinq_amnt","open_acc","default","num_months","purpose","sub_grade_num")
lc_keep_names = c("loan_id","borrower_id","loan_amount","pct_funded","loan_term","interest_rate","emp_length",
                  "home_ownership","annual_income","income_verified","accepted_date","issued_date","purpose","city","state",
                  "dti","delinquent_amount","inq_last_6mths","open_credit_lines","last_payment_month","months","credit_history_months","default","subgrade","fico")

loandata <- lc_raw[,names(lc_raw) %in% lc_keep]
names(loandata) <- lc_keep_names
loandata <- loandata[ , order(names(loandata))]
      loandata$last_payment_month <- ifelse(is.na(as.numeric(as.Date(loandata$last_payment_month, "%m/%d/%Y"))),loandata$issued_date,loandata$last_payment_month)
      loandata$pct_funded = loandata$pct_funded/loandata$loan_amount
      loandata$emp_length <- trim(loandata$emp_length)
      loandata$emp_length <- as.numeric(substr(loandata$emp_length,1,regexpr(' ', loandata$emp_length)[1]-1))*12
      loandata$income_verified <- ifelse(loandata$income_verified %in% c("Source Verified","Verified"), TRUE,
                                         ifelse(loandata$income_verified %in% c("Not Verified"),FALSE,NA))
      loandata$loan_term <- trim(loandata$loan_term)
      loandata$loan_term <- as.numeric(substr(loandata$loan_term,1,regexpr(' ', loandata$loan_term)[1]-1))
      loandata$home_ownership <- ifelse(loandata$home_ownership %in% c("MORTGAGE","OWN"),TRUE,
                                        ifelse(loandata$home_ownership %in% c("RENT"),FALSE,NA))
      loandata$accepted_date <- as.Date(loandata$accepted_date, "%m/%d/%Y")
      loandata$issued_date <- as.Date(loandata$issued_date, "%m/%d/%Y")
      loandata$city <- tolower(trim(loandata$city))
      loandata$state <- tolower(trim(loandata$state))
      loandata['data_date'] <- max(as.Date(loandata$last_payment_month, "%m/%d/%Y"),na.rm = TRUE)
      loandata['delinquent_days'] <- as.numeric(loandata$data_date - as.Date(loandata$last_payment_month, "%m/%d/%Y"))-30
      loandata$last_payment_month <- as.yearmon(as.Date(loandata$last_payment_month, "%m/%d/%Y"))
      loandata['lending_club'] <-1
      loandata <- loandata[loandata$months>1,]
      i <- sapply(loandata, is.factor)
      loandata[i] <- lapply(loandata[i], as.character)

prosper_raw = readRDS(file="E:/loan_stacking/prosper_raw.rds")
prosper_raw <- prosper_raw[!duplicated(prosper_raw),]
prosper_keep = c("LoanKey","MemberKey.x","LoanOriginalAmount","PercentFunded.x","Term","BorrowerRate.x","MonthsEmployed",
                 "IsBorrowerHomeowner","StatedMonthlyIncome.x","IncomeVerifiable.x","ListingCreationDate.y",
                 "LoanOriginationDate.x","BorrowerCity","BorrowerState.y","LoanCurrentDaysDelinquent","DebtToIncomeRatio","ProsperRating..numeric.",
                 "FirstRecordedCreditLine.x","FICO","InquiriesLast6Months.x","AmountDelinquent.x","OpenCreditLines.x","default","months")
prosper_keep_names = c("loan_term","interest_rate","subgrade","home_ownership","credit_history_months","open_credit_lines","inq_last_6mths","delinquent_amount","dti","income_verified",
                       "annual_income","loan_id","last_payment_month","loan_amount","issued_date","borrower_id","pct_funded",
                       "default","months","accepted_date","emp_length","state","city","fico")
loandata2 <- prosper_raw[,names(prosper_raw) %in% prosper_keep]
names(loandata2) <- prosper_keep_names
# loandata2 <- data.frame(lapply(loandata2, as.character), stringsAsFactors=FALSE)
loandata2 <- loandata2[ , order(names(loandata2))]
      loandata2$annual_income = as.numeric(loandata2$annual_income)*12
      loandata2$accepted_date = as.Date(substr(loandata2$accepted_date,1,10))
      loandata2$city <- tolower(trim(loandata2$city))
      loandata2$city <- ifelse(loandata2$city=="null",NA,loandata2$city)
      loandata2$issued_date <- trim(loandata2$issued_date)
      loandata2$issued_date <- as.Date(substr(loandata2$issued_date,1,regexpr(' ', loandata2$issued_date)[1]-1), "%m/%d/%Y")
      loandata2['data_date'] <- max(loandata2$issued_date,na.rm = TRUE)
      loandata2$credit_history_months <- as.Date(loandata2$credit_history_months, "%m/%d/%Y")
      loandata2$credit_history_months <- floor(as.numeric(loandata2$issued_date-loandata2$credit_history_months)/30)
      loandata2$dti <- as.numeric(loandata2$dti)*100
      loandata2$emp_length <- as.numeric(loandata2$emp_length)
      loandata2$fico <- as.numeric(loandata2$fico)
      loandata2$home_ownership <- ifelse(loandata2$home_ownership=="TRUE",TRUE,FALSE)
      loandata2$income_verified <- ifelse(loandata2$income_verified=="TRUE",TRUE,FALSE)
      loandata2['delinquent_days'] <- as.numeric(loandata2$last_payment_month)
      loandata2$last_payment_month <- as.numeric(loandata2$data_date) - loandata2$delinquent_days
      loandata2$last_payment_month <- as.yearmon(as.Date(loandata2$last_payment_month,origin = "1970-01-01"))
      loandata2$loan_amount <- as.numeric(loandata2$loan_amount)
      loandata2$loan_term <- as.numeric(loandata2$loan_term)
      loandata2$pct_funded <- as.numeric(loandata2$pct_funded)
      loandata2$state <- tolower(trim(loandata2$state))
      loandata2$state <- ifelse(loandata2$state=="null",NA,loandata2$state)
      loandata2$inq_last_6mths <- as.numeric(loandata2$inq_last_6mths)
      loandata2$delinquent_amount <- as.numeric(loandata2$delinquent_amount)
      loandata2['lending_club'] = 0
      loandata2$default <- ifelse(loandata2$delinquent_days>120,1,0)
      loandata2$months<- ifelse(loandata2$default==1,round((as.numeric(loandata2$data_date)-loandata2$delinquent_days-as.numeric(loandata2$accepted_date))/30,1),
                                round((as.numeric(loandata2$data_date)-120-as.numeric(loandata2$accepted_date))/30,1))
      loandata2 <- loandata2[loandata2$months>1,]
      i <- sapply(loandata2, is.factor)
      loandata2[i] <- lapply(loandata2[i], as.character)
      
loandata$purpose <- NULL      
loandata <- rbind(loandata,loandata2)

saveRDS(loandata,file="lc_prosper_merged_old_data.rds")

# summary(coxph(Surv(time = months,event = default)~interest_rate,data=loandata))
