setwd('~/wd')

library(plyr)
library(rvest)



#################################################################################



alllinks <- data.frame()

url1 <- 'https://www.charitynavigator.org/index.cfm?bay=search.alpha'

web1 <- read_html(url1, encoding = 'UTF-8') 
position1 <- web1 %>% html_nodes('p.letters a') 
link1 <- position1 %>% html_attrs()
link1 <- unlist(link1)
names(link1) <- NULL
link1 <- unique(link1)

for(i in 1:length(link1))
{
  cat(i, '\n')

   url2 <- link1[i]
   web2 <- read_html(url2, encoding = 'UTF-8')
   name2 <- web2 %>% html_nodes('div.mobile-padding') %>% html_text()
   name2 <- unlist(strsplit(gsub(' ', '', name2), '\r\n\r\n'))[-1]
   name2[1] <- strsplit(name2[1], '\t\t')[[1]][2]
   link2 <- web2 %>% html_nodes('div.mobile-padding a') %>% html_attrs()
   link2 <- unlist(link2)
   names(link2) <- NULL
   link2 <- link2[-1]
   
   temp <- data.frame(name = name2, link = link2)
   alllinks <- rbind(alllinks, temp)
  
}

alllinks$name <- as.character(alllinks$name)
alllinks$link <- as.character(alllinks$link)

save(alllinks, file = 'alllinks.rda')
write.csv(alllinks, file = 'AllLinks.csv', row.names = F)




#################################################################################



load('alllinks.rda')

basicdata <- data.frame()


for(i in 1:nrow(alllinks))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  
  #internal <- web1 %>% html_nodes('div.tabs a') %>% html_attrs()
  #sublink <- unlist(internal)
  #sublink <- sublink[3:5]
  #names(sublink) <- NULL
  #sublink[3] <- paste0('http://www.charitynavigator.org/', sublink[3])
  
  #web2 <- read_html(sublink[1], encoding = 'UTF-8')
  #web3 <- read_html(sublink[2], encoding = 'UTF-8')
  #web4 <- read_html(sublink[3], encoding = 'UTF-8')
  
  info1 <- web1 %>% html_nodes('div.cn-disappear p') %>% html_text()
  ind1 <- grep('EIN', info1)
  contact <- info1[ind1]
  ein <- gsub(' |\r|\n|\t', '', strsplit(contact, 'EIN:')[[1]][2])
  t1 <- strsplit(contact, 'EIN:')[[1]][1]
  fax <- gsub(' ', '', strsplit(t1, 'fax:')[[1]][2])
  t2 <- strsplit(t1, 'fax:')[[1]][1]
  tel <- gsub(' ', '', strsplit(t2, 'tel:')[[1]][2])
  t3 <- strsplit(t2, 'tel:')[[1]][1]
  charity <- gsub('\r|\n|\t', '', strsplit(t3, '\r\n\t\t\t')[[1]][1])
  t4 <- strsplit(t3, '\r\n\t\t\t')[[1]][-1]
  address <- ''
  for(i in 1:length(t4))
  {
    address <- paste(address, t4[i])
  }
  board <- info1[ind1+2]
  board <- gsub('\r|\n|    ', '', board)
  ceo <- info1[ind1+3]
  ceo <- gsub('\r|\n|', '', ceo)
  
  
  basicdata <- rbind(basicdata, data.frame(charity, ein, address, tel, fax, board, ceo))
  
}



save(basicdata, file = 'basicdata.rda')
write.csv(basicdata, file = 'BasicData.csv', row.names = F)



#################################################################################



load('basicdata.rda')


for(i in 1:nrow(basicdata))
{
  cat(i, '\n')
  
  basicdata$tel[i] <- gsub('\r|\n', '', strsplit(as.character(basicdata$tel[i]), 'TTY')[[1]][1])
  basicdata$fax[i] <- gsub('\r|\n', '', strsplit(as.character(basicdata$fax[i]), 'TTY')[[1]][1])
  
  
}


save(basicdata, file = 'basicdata1.rda')


load('basicdata1.rda')

basicdata$board <- as.character(basicdata$board)
basicdata$ceo <- as.character(basicdata$ceo)


for(i in 1:nrow(basicdata))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  check1 <- web1 %>% html_nodes('h1')
  info1 <- web1 %>% html_nodes('p')
  t1 <- info1[grep('<strong>', info1)]
  
  if(length(grep('CEO', check1)) != 0 & length(grep('Board Leadership', check1)) != 0)
  {
    ceo <- t1[length(t1)-1]
    ceo <- gsub('\r|\n|  ', '', ceo %>% html_text())
    board <- t1[length(t1)-2]
    board <- gsub('\r|\n|  ', '', board %>% html_text())

    basicdata$ceo[i] <- ceo
    basicdata$board[i] <- board
  } 
  
  if(length(grep('CEO', check1)) != 0 & length(grep('Board Leadership', check1)) == 0)
  {
    ceo <- t1[length(t1)-1]
    ceo <- gsub('\r|\n|  ', '', ceo %>% html_text())
    
    basicdata$ceo[i] <- ceo
    basicdata$board[i] <- NA
  } 
  
  if(length(grep('CEO', check1)) == 0 & length(grep('Board Leadership', check1)) != 0)
  {
    board <- t1[length(t1)-1]
    board <- gsub('\r|\n|  ', '', board %>% html_text())
    
    basicdata$ceo[i] <- NA
    basicdata$board[i] <- board
  } 
  
  if(length(grep('CEO', check1)) == 0 & length(grep('Board Leadership', check1)) == 0)
  {
    basicdata$ceo[i] <- NA
    basicdata$board[i] <- NA
  } 
  
  
}



save(basicdata, file = 'basicdata2.rda')


write.csv(basicdata, file = 'BasicData.csv', row.names = F)




#################################################################################


load('alllinks.rda')
load('basicdata2.rda')

ratingprofiledata1 <- data.frame()



for(i in 1:nrow(alllinks))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  
  info1 <- web1 %>% html_nodes('table')
  
  temp <- info1[[1]] %>% html_table(fill=TRUE)
  
  if(nrow(temp) == 5)
  {
    overall <- temp[3,2]
    financial <- temp[4,2]
    accountability_transparency <- temp[5,2]

  } else {
    overall <- NA
    financial <- NA
    accountability_transparency <- NA
    
  }
  
  Charity <- basicdata$Charity[i]
  EIN <- basicdata$EIN[i]
  
  ratingprofiledata1 <- rbind(ratingprofiledata1, data.frame(Charity, EIN, overall, financial, accountability_transparency))

  
}


save(ratingprofiledata1, file = 'ratingprofiledata1.rda')
write.csv(ratingprofiledata1, file = 'RatingProfileData1.csv', row.names = F)



load('ratingprofiledata1.rda')

tempdata <- data.frame()


for(i in 1:nrow(alllinks))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  
  info1 <- web1 %>% html_nodes('table')
  
  temp <- info1[[2]] %>% html_table(fill=TRUE)
  
  if(nrow(temp) == 3)
  {
    Overall_Rating <- temp[1,3]
    Financial_Rating <- temp[2,3]
    Accountability_Transparency_Rating <- temp[3,3]
    
  } else {
    Overall_Rating <- NA
    Financial_Rating <- NA
    Accountability_Transparency_Rating <- NA
    
  }
  
  Charity <- basicdata$Charity[i]
  EIN <- basicdata$EIN[i]
  
  tempdata <- rbind(tempdata, data.frame(Charity, EIN, Overall_Rating, Financial_Rating, Accountability_Transparency_Rating))
  
}


ratingprofiledata11 <- cbind(ratingprofiledata1, tempdata)
ratingprofiledata11[,6:7] <- NULL
names(ratingprofiledata11)[3:5] <- c('Overall_Score', 'Financial_Score', 'Accountability_Transparency_Score')


save(ratingprofiledata11, file = 'ratingprofiledata11.rda')
write.csv(ratingprofiledata11, file = 'RatingProfileData1.csv', row.names = F)



#################################################################################


load('alllinks.rda')
load('basicdata2.rda')

ratingprofiledata2 <- data.frame()



for(i in 1:nrow(alllinks))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  
  info1 <- web1 %>% html_nodes('table')
  
  
  if(length(info1) >= 3)
  {
    temp <- info1[[3]] %>% html_table(fill=TRUE)
    
    program_expenses <- temp[1,3]
    administrative_expenses <- temp[2,3]
    fundraising_expenses <- temp[3,3]
    fundraising_efficiency <- temp[4,3]
    working_capital_ratio_years <- temp[5,3]
    program_expenses_growth <- temp[6,3]
    liabilities_to_assets <- temp[7,3]
  
    
  } else {
    program_expenses <- NA
    administrative_expenses <- NA
    fundraising_expenses <- NA
    fundraising_efficiency <- NA
    working_capital_ratio_years <- NA
    program_expenses_growth <- NA
    liabilities_to_assets <- NA
    
  }
  
  
  Charity <- basicdata$Charity[i]
  EIN <- basicdata$EIN[i]
  
  ratingprofiledata2 <- rbind(ratingprofiledata2, data.frame(Charity, EIN, program_expenses, administrative_expenses, fundraising_expenses, fundraising_efficiency, working_capital_ratio_years, program_expenses_growth, liabilities_to_assets))
  
  
}



save(ratingprofiledata2, file = 'ratingprofiledata2.rda')
write.csv(ratingprofiledata2, file = 'RatingProfileData2.csv', row.names = F)



#################################################################################



load('alllinks.rda')
load('basicdata2.rda')

ratingprofiledata3 <- data.frame()


for(i in 1:nrow(alllinks))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  
  info1 <- web1 %>% html_nodes('img')
  info2 <- unlist(info1 %>% html_attrs())
  names(info2) <- NULL
  info3 <- info2[grep('https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/', info2)]
  
  if(length(info3) >= 17)
  {
    Independent_Voting_Board_Members <- ifelse(info3[1] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[1] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    No_Material_diversion_of_assets <- ifelse(info3[2] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[2] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Audited_financials_prepared_by_independent_accountant <- ifelse(info3[3] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[3] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Does_Not_Provide_Loans_to_or_Receive_Loans_From_related_parties <- ifelse(info3[4] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[4] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Documents_Board_Meeting_Minutes <- ifelse(info3[5] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[5] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Provided_copy_of_Form990_to_organization_governing_body_before_filing <- ifelse(info3[6] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[6] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Conflict_of_Interest_Policy <- ifelse(info3[7] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[7] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Whistleblower_Policy <- ifelse(info3[8] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[8] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Records_Retention_and_Destruction_Policy <- ifelse(info3[9] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[9] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    CEO_listed_with_salary <- ifelse(info3[10] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[10] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Process_for_determining_CEO_compensation <- ifelse(info3[11] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[11] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Board_Listed_or_Board_Members_Not_Compensated <- ifelse(info3[12] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[12] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Donor_Privacy_Policy <- ifelse(info3[13] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[13] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Board_Members_Listed <- ifelse(info3[14] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[14] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Audited_Financials <- ifelse(info3[15] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[15] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Form990 <- ifelse(info3[16] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[16] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    Key_staff_listed <- ifelse(info3[17] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checked.gif',1, ifelse(info3[17] == 'https://d20umu42aunjpx.cloudfront.net/_gfx_/icons/checkboxX.gif', -1, 0))
    
  } else {
    Independent_Voting_Board_Members <- NA
    No_Material_diversion_of_assets <- NA
    Audited_financials_prepared_by_independent_accountant <- NA
    Does_Not_Provide_Loans_to_or_Receive_Loans_From_related_parties <- NA
    Documents_Board_Meeting_Minutes <- NA
    Provided_copy_of_Form990_to_organization_governing_body_before_filing <- NA
    Conflict_of_Interest_Policy <- NA
    Whistleblower_Policy <- NA
    Records_Retention_and_Destruction_Policy <- NA
    CEO_listed_with_salary <- NA
    Process_for_determining_CEO_compensation <- NA
    Board_Listed_or_Board_Members_Not_Compensated <- NA
    Donor_Privacy_Policy <- NA
    Board_Members_Listed <- NA
    Audited_Financials <- NA
    Form990 <- NA
    Key_staff_listed <- NA
    
  }
  
  
  Charity <- basicdata$Charity[i]
  EIN <- basicdata$EIN[i]
  
  ratingprofiledata3 <- rbind(ratingprofiledata3, data.frame(Charity, EIN, Independent_Voting_Board_Members,
                                                             No_Material_diversion_of_assets,
                                                             Audited_financials_prepared_by_independent_accountant,
                                                             Does_Not_Provide_Loans_to_or_Receive_Loans_From_related_parties,
                                                             Documents_Board_Meeting_Minutes,
                                                             Provided_copy_of_Form990_to_organization_governing_body_before_filing,
                                                             Conflict_of_Interest_Policy,
                                                             Whistleblower_Policy,
                                                             Records_Retention_and_Destruction_Policy,
                                                             CEO_listed_with_salary,
                                                             Process_for_determining_CEO_compensation,
                                                             Board_Listed_or_Board_Members_Not_Compensated,
                                                             Donor_Privacy_Policy,
                                                             Board_Members_Listed,
                                                             Audited_Financials,
                                                             Form990,
                                                             Key_staff_listed))
  
  
}



save(ratingprofiledata3, file = 'ratingprofiledata3.rda')
write.csv(ratingprofiledata3, file = 'RatingProfileData3.csv', row.names = F)



#################################################################################


load('alllinks.rda')
load('basicdata2.rda')

ratingprofiledata4 <- data.frame()



for(i in 1:nrow(alllinks))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  
  info1 <- web1 %>% html_nodes('table')
  
  
  if(length(info1) >= 6)
  {
    temp <- info1[[6]] %>% html_table(fill=TRUE)
    
    Contributions_Gifts_Grants <- temp[3,2]
    Federated_Campaigns <- temp[4,2]
    Membership_Dues <- temp[5,2]
    Fundraising_Events <- temp[6,2]
    Related_Organizations <- temp[7,2]
    Government_Grants <- temp[8,2]
    Total_Contributions <- temp[9,2]
    Program_Service_Revenue <- temp[10,2]
    Total_Primary_Revenue <- temp[11,2]
    Other_Revenue <- temp[12,2]
    TOTAL_REVENUE <- temp[13,2]
    Program_Expenses <- temp[16,2]
    Administrative_Expenses <- temp[17,2]
    Fundraising_Expenses <- temp[18,2]
    TOTAL_FUNCTIONAL_EXPENSES <- temp[19,2]
    Payments_to_Affiliates <- temp[21,2]
    Excess_or_Deficit_for_the_year <- temp[22,2]
    Net_Assets <- temp[24,2]
    
    
  } else {
    Contributions_Gifts_Grants <- NA
    Federated_Campaigns <- NA
    Membership_Dues <- NA
    Fundraising_Events <- NA
    Related_Organizations <- NA
    Government_Grants <- NA
    Total_Contributions <- NA
    Program_Service_Revenue <- NA
    Total_Primary_Revenue <- NA
    Other_Revenue <- NA
    TOTAL_REVENUE <- NA
    Program_Expenses <- NA
    Administrative_Expenses <- NA
    Fundraising_Expenses <- NA
    TOTAL_FUNCTIONAL_EXPENSES <- NA
    Payments_to_Affiliates <- NA
    Excess_or_Deficit_for_the_year <- NA
    Net_Assets <- NA

    }
  
  
  Charity <- basicdata$Charity[i]
  EIN <- basicdata$EIN[i]
  
  ratingprofiledata4 <- rbind(ratingprofiledata4, data.frame(Charity, EIN, Contributions_Gifts_Grants, Federated_Campaigns, Membership_Dues, Fundraising_Events, Related_Organizations, Government_Grants, Total_Contributions, Program_Service_Revenue, Total_Primary_Revenue, Other_Revenue, TOTAL_REVENUE, Program_Expenses, Administrative_Expenses, Fundraising_Expenses, TOTAL_FUNCTIONAL_EXPENSES, Payments_to_Affiliates, Excess_or_Deficit_for_the_year, Net_Assets))
  
  
}


save(ratingprofiledata4, file = 'ratingprofiledata4.rda')
write.csv(ratingprofiledata4, file = 'RatingProfileData4.csv', row.names = F)



#################################################################################



load('alllinks.rda')
load('basicdata2.rda')

ratingprofiledata5 <- data.frame()


for(i in 1:nrow(alllinks))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  
  info1 <- web1 %>% html_nodes('table')
  
  
  if(length(info1) >= 10)
  {
    temp <- info1[[10]] %>% html_table(fill=TRUE)
    
    if(ncol(temp) != 5)
    {
      temp <- info1[[9]] %>% html_table(fill=TRUE)
    }
    
    Compensation_of_Leaders <- temp[1,1]
    Percentage_of_Expenses_Leaders <- temp[1,2]
    Paid_to_Name_Leaders <- temp[1,3]
    Paid_to_Title_Leaders <- temp[1,4]
    
    if(nrow(temp) == 3)
    {
      Other_Salaries_of_Note <- temp[3,1]
      Percentage_of_Expenses_Other <- temp[3,2]
      Paid_to_Name_Other <- temp[3,3]
      Paid_to_Title_Other <- temp[3,4]
    } else {
      Other_Salaries_of_Note <- NA
      Percentage_of_Expenses_Other <- NA
      Paid_to_Name_Other <- NA
      Paid_to_Title_Other <- NA
    }
    
    
  } else {
    
    Compensation_of_Leaders <- NA
    Percentage_of_Expenses_Leaders <- NA
    Paid_to_Name_Leaders <- NA
    Paid_to_Title_Leaders <- NA
    
    Other_Salaries_of_Note <- NA
    Percentage_of_Expenses_Other <- NA
    Paid_to_Name_Other <- NA
    Paid_to_Title_Other <- NA
    
  }
  
  
  Charity <- basicdata$Charity[i]
  EIN <- basicdata$EIN[i]
  
  ratingprofiledata5 <- rbind(ratingprofiledata5, data.frame(Charity, EIN, Compensation_of_Leaders, Percentage_of_Expenses_Leaders, Paid_to_Name_Leaders, Paid_to_Title_Leaders, Other_Salaries_of_Note, Percentage_of_Expenses_Other, Paid_to_Name_Other, Paid_to_Title_Other))
  
  
}


save(ratingprofiledata5, file = 'ratingprofiledata5.rda')
write.csv(ratingprofiledata5, file = 'RatingProfileData5.csv', row.names = F)




#################################################################################



load('alllinks.rda')
load('basicdata2.rda')

ratingprofiledata6 <- data.frame()



for(i in 1:nrow(alllinks))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  
  info1 <- web1 %>% html_nodes('table')
  
  
  if(length(info1) >= 5)
  {
    temp <- info1[[5]] %>% html_table(fill=TRUE)
    
    Highly_Rated_Similar_Charity_1 <- temp[2,1]
    Highly_Rated_Similar_Charity_1_Overall_Score <- temp[2,2]
    Highly_Rated_Similar_Charity_1_Overall_Rating <- temp[2,3]
    Highly_Rated_Similar_Charity_2 <- temp[3,1]
    Highly_Rated_Similar_Charity_2_Overall_Score <- temp[3,2]
    Highly_Rated_Similar_Charity_2_Overall_Rating <- temp[3,3]
    Highly_Rated_Similar_Charity_3 <- temp[4,1]
    Highly_Rated_Similar_Charity_3_Overall_Score <- temp[4,2]
    Highly_Rated_Similar_Charity_3_Overall_Rating <- temp[4,3]
    Highly_Rated_Similar_Charity_4 <- temp[5,1]
    Highly_Rated_Similar_Charity_4_Overall_Score <- temp[5,2]
    Highly_Rated_Similar_Charity_4_Overall_Rating <- temp[5,3]
    
    
  } else {
    Highly_Rated_Similar_Charity_1 <- NA
    Highly_Rated_Similar_Charity_1_Overall_Score <- NA
    Highly_Rated_Similar_Charity_1_Overall_Rating <- NA
    Highly_Rated_Similar_Charity_2 <- NA
    Highly_Rated_Similar_Charity_2_Overall_Score <- NA
    Highly_Rated_Similar_Charity_2_Overall_Rating <- NA
    Highly_Rated_Similar_Charity_3 <- NA
    Highly_Rated_Similar_Charity_3_Overall_Score <- NA
    Highly_Rated_Similar_Charity_3_Overall_Rating <- NA
    Highly_Rated_Similar_Charity_4 <- NA
    Highly_Rated_Similar_Charity_4_Overall_Score <- NA
    Highly_Rated_Similar_Charity_4_Overall_Rating <- NA
    
  }
  
  
  Charity <- basicdata$Charity[i]
  EIN <- basicdata$EIN[i]
  
  ratingprofiledata6 <- rbind(ratingprofiledata6, data.frame(Charity, EIN, Highly_Rated_Similar_Charity_1, Highly_Rated_Similar_Charity_1_Overall_Score, Highly_Rated_Similar_Charity_1_Overall_Rating, Highly_Rated_Similar_Charity_2, Highly_Rated_Similar_Charity_2_Overall_Score, Highly_Rated_Similar_Charity_2_Overall_Rating, Highly_Rated_Similar_Charity_3, Highly_Rated_Similar_Charity_3_Overall_Score, Highly_Rated_Similar_Charity_3_Overall_Rating, Highly_Rated_Similar_Charity_4, Highly_Rated_Similar_Charity_4_Overall_Score, Highly_Rated_Similar_Charity_4_Overall_Rating))
  
  
}



save(ratingprofiledata6, file = 'ratingprofiledata6.rda')
write.csv(ratingprofiledata6, file = 'RatingProfileData6.csv', row.names = F)



#################################################################################


load('alllinks.rda')
load('basicdata2.rda')

IRSdata <- data.frame()



for(i in 1:nrow(alllinks))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  
  info1 <- web1 %>% html_nodes('table')
  
  Name_in_IRS_Master_File <- NA
  NTEE_Code <- NA
  NTEE_Classification <- NA
  NTEE_Type <- NA
  Classification <- NA
  Subsection <- NA
  Activities <- NA
  Foundation_Status <- NA
  Deductibility <- NA
  Affiliation <- NA
  Group_Name <- NA
  Ruling_Date <- NA
  Filing_Requirement <- NA
  Fiscal_Year_End <- NA
  IRS_Forms990 <- NA
  
  
  for(j in 1:length(info1))
  {
    temp <- info1[[j]] %>% html_table(fill=TRUE)
    
    if(nrow(temp) == 16 & ncol(temp) == 2)
    {
      Name_in_IRS_Master_File <- temp[2,2]
      NTEE_Code <- temp[3,2]
      NTEE_Classification <- temp[4,2]
      NTEE_Type <- temp[5,2]
      Classification <- temp[6,2]
      Subsection <- temp[7,2]
      Activities <- temp[8,2]
      Foundation_Status <- temp[9,2]
      Deductibility <- temp[10,2]
      Affiliation <- temp[11,2]
      Group_Name <- temp[12,2]
      Ruling_Date <- temp[13,2]
      Filing_Requirement <- temp[14,2]
      Fiscal_Year_End <- temp[15,2]
      IRS_Forms990 <- temp[16,2]
      
      break
    }
    
  }
    
  
  Charity <- basicdata$Charity[i]
  EIN <- basicdata$EIN[i]
  
  IRSdata <- rbind(IRSdata, data.frame(Charity, EIN, Name_in_IRS_Master_File,
                                       NTEE_Code,
                                       NTEE_Classification,
                                       NTEE_Type,
                                       Classification,
                                       Subsection,
                                       Activities,
                                       Foundation_Status,
                                       Deductibility,
                                       Affiliation,
                                       Group_Name,
                                       Ruling_Date,
                                       Filing_Requirement,
                                       Fiscal_Year_End,
                                       IRS_Forms990))
  
  
}



save(IRSdata, file = 'IRSdata.rda')
write.csv(IRSdata, file = 'IRSData.csv', row.names = F)



#################################################################################


load('alllinks.rda')
load('basicdata2.rda')


programdata <- data.frame()


for(i in 1:nrow(alllinks))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  
  info1 <- web1 %>% html_nodes('table')
  
  Charity <- basicdata$Charity[i]
  EIN <- basicdata$EIN[i]
  
  mark <- 0
  
  
  for(j in 1:length(info1))
  {
    temp <- info1[[j]] %>% html_table(fill=TRUE)
    
    if(names(temp)[1] == 'Program Name')
    {
      temp2 <- unlist(temp)
      Program.Number <- nrow(temp)
      tempdata <- data.frame(Charity, EIN, Program.Number, t(temp2))
      mark <- 1
      break
    }
    
  }
  
  if(ncol(tempdata) == 6)
  {
    names(tempdata)[4:6] <- c('Program.Name1', 'Amount.Spent1', 'X..of.Program.Expenses1')
    
  }
  
  if(mark == 1)
  {
    programdata <- rbind.fill(programdata, tempdata)
    
  } else {
    Program.Number <- 0
    Program.Name1 <- NA
    Amount.Spent1 <- NA
    X..of.Program.Expenses1 <- NA
    programdata <- rbind.fill(programdata, data.frame(Charity, EIN, Program.Number, Program.Name1, Amount.Spent1, X..of.Program.Expenses1))
  }
  
  

}



save(programdata, file = 'Programdata.rda')
write.csv(programdata, file = 'ProgramData.csv', row.names = F)



#################################################################################


load('alllinks.rda')
load('basicdata2.rda')


historicaldata <- data.frame()



for(i in 1:nrow(alllinks))
{
  cat(i, '\n')
  
  web1 <- read_html(alllinks$link[i], encoding = 'UTF-8')
  
  info1 <- web1 %>% html_nodes('table')
  
  Charity <- basicdata$Charity[i]
  EIN <- basicdata$EIN[i]
  
  mark <- 0
  
  
  for(j in 1:length(info1))
  {
    temp <- info1[[j]] %>% html_table(fill=TRUE)
    
    if(names(temp)[1] == 'Form 990 FYE')
    {
      temp2 <- temp[,1:4]
      ind2 <- which(temp2[,1]=='')
      if(length(ind2) != 0)
      {
        temp3 <- temp2[-ind2,]
      } else {
        temp3 <- temp2
      }
      
      ind3 <- grep('CN',temp3[,1])
      ind4 <- diff(c(ind3,nrow(temp3)+1)) - 1
      CN <- rep(temp3[ind3,1], ind4)
      temp4 <- temp3[-ind3,]
      tempdata <- cbind(temp4, CN, Charity, EIN)
      mark <- 1
      break
    }
    
  }
  
  
  if(mark == 1)
  {
    historicaldata <- rbind(historicaldata, tempdata)
    
  } else {
    
    tempdata2 <- data.frame(NA, NA, NA, NA, NA, Charity, EIN)
    names(tempdata2)[1:5] <- c('Form 990 FYE', 'Date Published', 'Overall Score', 'Overall Rating', 'CN')
    historicaldata <- rbind(historicaldata, tempdata2)
  }
  
  
  
}



save(historicaldata, file = 'historicaldata.rda')
write.csv(historicaldata, file = 'HistoricalData.csv', row.names = F)




#################################################################################




