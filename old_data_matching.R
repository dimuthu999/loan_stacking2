rm(list=ls())
path = "E:/loan_stacking/"
setwd(path)
library(plyr)
library(psych)
library(stargazer)
library(Matching)
library(sandwich)
library(survival)
library(lubridate)
library(zoo)
output.type="text"
# following file was created by the script lc_prosper_merge.R available at https://github.com/dimuthu999/loan_stacking2/blob/master/lc_prosper_merge.R
loandata <- readRDS(file="lc_prosper_merged_old_data.rds")

# 
# city_data <- ddply(loandata,.(state,city),summarise,obs=length(loan_id))
# saveRDS(city_data,file="city_data.rds")

city_data <- readRDS(file = "city_data.rds")
loandata <- merge(loandata,city_data,by=c("state","city"))
loandata <- loandata[!duplicated(loandata),]

mortgage <- function(P, I, L) { 
  J <- I/(12 * 100)
  N <- 1 * L
  M <- P*J/(1-(1+J)^(-N))
  return(M)
}


# Matching Small Cities ---------------------------------------------------

small <- loandata[loandata$obs<=200,]
small['payment'] <- mortgage(small$loan_amount,small$interest_rate*100,small$loan_term)
small['competitor_payment'] <- NULL
small['competitor_interest'] <- NULL
small['competitor_loanamount'] <- NULL

matched_index = 1
small['accepted_m2'] <-ifelse(small$lending_club==1,small$accepted_date,small$accepted_date+0)
small['key'] <- (paste(small$city,format(small$accepted_date,"%Y"),small$state,floor(small$annual_income/10000),small$accepted_m2,sep = ""))
prosper_loans <- small[small$lending_club==0,]
  lc_loans <- small[small$lending_club==1,]
  lc_loans2 <- small[small$lending_club==1,]
  lc_loans2$accepted_date = lc_loans2$accepted_date+1
  lc_loans3 <- small[small$lending_club==1,]
  lc_loans3$accepted_date = lc_loans3$accepted_date-1
  lc_loans<- rbind(lc_loans,lc_loans2,lc_loans3)
prosper_loans <- prosper_loans[prosper_loans$key %in% unique(lc_loans$key),]
lc_loans <- lc_loans[lc_loans$key %in% unique(prosper_loans$key),]

matched_data <- NULL
for(i in 1:nrow(prosper_loans))  {
  cat(i, " of ",nrow(prosper_loans)," ",matched_index,"\n")
  temp <- lc_loans[ lc_loans$key==prosper_loans[i,]$key ,]

  # temp <- temp[!is.na(temp$zip),]
  if(nrow(temp)>1) {
    temp = temp[which.min(abs(temp$annual_income-prosper_loans[i,]$annual_income)),]
  }
  if(nrow(temp)==1) {
    matched_pair <- rbind(prosper_loans[i,],temp)
    matched_pair['matched_index'] = matched_index
    # 
    # matched_pair[1,'competitor_payment'] <- matched_pair[2,'payment']
    # matched_pair[2,'competitor_payment'] <- matched_pair[1,'payment']
    # matched_pair[1,'competitor_interest'] <- matched_pair[2,'interest_rate']
    # matched_pair[2,'competitor_interest'] <- matched_pair[1,'interest_rate']
    # matched_pair[1,'competitor_loanamount'] <- matched_pair[2,'loan_amount']
    # matched_pair[2,'competitor_loanamount'] <- matched_pair[1,'loan_amount']
    
    matched_data <- rbind(matched_data,matched_pair)
    matched_index = matched_index+1
  }
}

loandata <- loandata[loandata$obs<=200,]
loandata['stacked'] <- ifelse(loandata$loan_id %in% c(matched_data$loan_id),1,0)



loandata['loan_to_income'] <- loandata$loan_amount/loandata$annual_income
loandata['term_60'] <- ifelse(loandata$loan_term==60,1,0)
loandata$income_verified <- ifelse(loandata$income_verified==TRUE,1,0)
loandata <- loandata[is.finite(loandata$loan_to_income),]

loandata['age'] <- round(as.numeric(loandata$data_date - loandata$accepted_date)/30)
loandata <- loandata[loandata$age<60,]

loandata['accepted_month'] <- as.yearmon(loandata$accepted_date)
loandata['fraud'] <- ifelse(loandata$months <=6  & loandata$default==1,1,0)


# Revise Credit Variables -------------------------------------------------
mortgage <- function(P, I, L) { 
  J <- I/(12 * 100)
  N <- 1 * L
  M <- P*J/(1-(1+J)^(-N))
  return(M)
}


matched_data['payment'] <- mortgage(matched_data$loan_amount,matched_data$interest_rate*100,matched_data$loan_term)
loandata['payment'] <- mortgage(loandata$loan_amount,loandata$interest_rate*100,loandata$loan_term)
loandata['payment2'] <- 0
loandata['revised_credit_lines'] <- 0
loandata['revised_loan_amount'] <- 0
loandata['competitor_loan_amount'] <- 0

for(i in 1:nrow(matched_data)) {
  cat(i," ")
  tryCatch({
    if(matched_data[i,]$lending_club==0) {
      loandata[loandata$loan_id == matched_data[matched_data$matched_index== matched_data[i,]$matched_index & matched_data$lending_club==1,]$loan_id & loandata$lending_club==1,]$payment2 = matched_data[i,]$payment
      loandata[loandata$loan_id == matched_data[matched_data$matched_index== matched_data[i,]$matched_index & matched_data$lending_club==1,]$loan_id & loandata$lending_club==1,]$revised_credit_lines = 1
      loandata[loandata$loan_id == matched_data[matched_data$matched_index== matched_data[i,]$matched_index & matched_data$lending_club==1,]$loan_id & loandata$lending_club==1,]$revised_loan_amount = matched_data[i,]$loan_amount
    }
    if(matched_data[i,]$lending_club==1) {
      loandata[loandata$loan_id == matched_data[matched_data$matched_index== matched_data[i,]$matched_index & matched_data$lending_club==0,]$loan_id & loandata$lending_club==0,]$payment2 = matched_data[i,]$payment
      loandata[loandata$loan_id == matched_data[matched_data$matched_index== matched_data[i,]$matched_index & matched_data$lending_club==0,]$loan_id & loandata$lending_club==0,]$revised_credit_lines=1
      loandata[loandata$loan_id == matched_data[matched_data$matched_index== matched_data[i,]$matched_index & matched_data$lending_club==0,]$loan_id & loandata$lending_club==0,]$revised_loan_amount=matched_data[i,]$loan_amount
    }
  },error=function(cond) {print("error")})
}

loandata['revised_dti'] <- ((loandata$annual_income/12)*(loandata$dti/100)+loandata$payment2)/(loandata$annual_income/12)
loandata['revised_credit_lines'] = loandata$open_credit_lines+loandata$revised_credit_lines
loandata['competitor_loan_amount'] = loandata$revised_loan_amount
loandata['revised_loan_amount'] = loandata$loan_amount + loandata$revised_loan_amount
loandata['revised_loan_to_income'] = loandata$revised_loan_amount/loandata$annual_income
loandata['competitor_loan_to_income'] = loandata$competitor_loan_amount/loandata$annual_income

t1 <- loandata[loandata$stacked==1,]
temp_ols <- lm(competitor_loan_to_income~loan_to_income,data=t1)
t1['resid_com_loan_to_income'] <- temp_ols$residuals
loandata['resid_com_loan_to_income']=0

loandata <- rbind(loandata[loandata$stacked==0,],t1)
# loandata['resid_com_loan_to_income'] <- ifelse(loandata$stacked==1,loandata$competitor_loan_to_income-loandata$loan_to_income*temp_ols$coefficients[2]+temp_ols$coefficients[1],0)


saveRDS(loandata,file="cleand_old_data_Apr102017.rds")
# Regressions -------------------------------------------------------------


library(survival)
library(stargazer)





cox_formula <- as.formula("Surv(months,default)~factor(stacked)+dti+loan_to_income+open_credit_lines+annual_income+credit_history_months+emp_length+factor(home_ownership)+loan_amount+factor(income_verified)+factor(term_60)+inq_last_6mths+factor(state)")
cox_formula2 <- as.formula("Surv(months,default)~factor(stacked)+dti+loan_to_income+open_credit_lines+annual_income+credit_history_months+emp_length+factor(home_ownership)+loan_amount+factor(income_verified)+factor(term_60)+inq_last_6mths+factor(lending_club)+factor(state)")

cox <- list()
cox[[1]] <- coxph(cox_formula2,data = loandata)
cox[[2]] <- coxph(cox_formula,data = loandata[loandata$lending_club==1,])
cox[[3]] <- coxph(cox_formula,data = loandata[loandata$lending_club==0,])

stargazer(cox,type = output.type,omit = c("state"),omit.labels = c("state"),omit.stat = c("ll","logrank","max.rsq","lr","wald"),no.space = TRUE,title="COXPH")




lm_1 <- as.formula("fraud~factor(stacked)+dti+loan_to_income+open_credit_lines+annual_income+credit_history_months+emp_length+factor(home_ownership)+loan_amount+factor(income_verified)+factor(term_60)+inq_last_6mths+factor(state)")
lm_2 <- as.formula("fraud~factor(stacked)+dti+loan_to_income+open_credit_lines+annual_income+credit_history_months+emp_length+factor(home_ownership)+loan_amount+factor(income_verified)+factor(term_60)+inq_last_6mths+factor(lending_club)+factor(state)")

cox <- list()
cox[[1]] <- glm(lm_2,data = loandata,family = "binomial")
cox[[2]] <- glm(lm_1,data = loandata[loandata$lending_club==1,],family = "binomial")
cox[[3]] <- glm(lm_1,data = loandata[loandata$lending_club==0,],family = "binomial")

stargazer(cox,type = output.type,omit = c("state"),omit.labels = c("state"),omit.stat = c("ll","logrank","max.rsq","lr","wald"),no.space = TRUE,title="Logistic Regression")




cox_formula <- as.formula("Surv(months,default)~factor(stacked)+interest_rate+factor(state)")
cox_formula2 <- as.formula("Surv(months,default)~factor(stacked)+interest_rate*factor(lending_club)+factor(state)")

cox <- list()
cox[[1]] <- coxph(cox_formula2,data = loandata)
cox[[2]] <- coxph(cox_formula,data = loandata[loandata$lending_club==1,])
cox[[3]] <- coxph(cox_formula,data = loandata[loandata$lending_club==0,])

stargazer(cox,type = output.type,omit = c("state","accepted_year"),omit.labels = c("state","accepted year"),omit.stat = c("ll","logrank","max.rsq","lr","wald"),no.space = TRUE,title="COXPH",column.labels = c("Both","Lending Club","Prosper"))





def_formula <- as.formula("loan_amount~factor(stacked)+factor(state)")
def_formula2 <- as.formula("loan_amount~factor(stacked)+factor(state)+factor(lending_club)")

ols <- list()
ols[[1]] <- lm(def_formula2,data = loandata)
ols[[2]] <- lm(def_formula,data = loandata[loandata$lending_club==1,])
ols[[3]] <- lm(def_formula,data = loandata[loandata$lending_club==0,])

stargazer(ols,type = "text",omit = c("state","accepted_year"),omit.labels = c("state","accepted year"),omit.stat = c("f","rsq","ser"),title = "Loan Amount",no.space = TRUE,column.labels = c("Both","Lending Club","Prosper"))




cox_formula1 <- as.formula("Surv(months,default)~factor(stacked)+revised_dti+revised_loan_to_income+revised_credit_lines+annual_income+credit_history_months+emp_length+factor(home_ownership)+loan_amount+factor(income_verified)+factor(loan_term)+inq_last_6mths+factor(state)")
cox_formula2 <- as.formula("Surv(months,default)~factor(stacked)+revised_dti+revised_loan_to_income+revised_credit_lines+annual_income+credit_history_months+emp_length+factor(home_ownership)+loan_amount+factor(income_verified)+factor(loan_term)+inq_last_6mths+factor(state)+factor(lending_club)")


cox <- list()
cox[[1]] <- coxph(cox_formula2,data = loandata)
cox[[2]] <- coxph(cox_formula1,data = loandata[loandata$lending_club==1,])
cox[[3]] <- coxph(cox_formula1,data = loandata[loandata$lending_club==0,])

stargazer(cox,type = "text",omit = c("state"),omit.labels = c("state"),omit.stat = c("ll","logrank","max.rsq","lr","wald"),no.space = TRUE,title="COXPH")



cox_formula <- as.formula("Surv(months,default)~factor(stacked)+resid_com_loan_to_income+dti+loan_to_income+open_credit_lines+annual_income+credit_history_months+emp_length+factor(home_ownership)+loan_amount+factor(income_verified)+factor(term_60)+inq_last_6mths+factor(state)")
cox_formula2 <- as.formula("Surv(months,default)~factor(stacked)+resid_com_loan_to_income+dti+loan_to_income+open_credit_lines+annual_income+credit_history_months+emp_length+factor(home_ownership)+loan_amount+factor(income_verified)+factor(term_60)+inq_last_6mths+factor(lending_club)+factor(state)")

cox <- list()
cox[[1]] <- coxph(cox_formula2,data = loandata)
cox[[2]] <- coxph(cox_formula,data = loandata[loandata$lending_club==1,])
cox[[3]] <- coxph(cox_formula,data = loandata[loandata$lending_club==0,])

stargazer(cox,type = output.type,omit = c("state"),omit.labels = c("state"),omit.stat = c("ll","logrank","max.rsq","lr","wald"),no.space = TRUE,title="COXPH")

