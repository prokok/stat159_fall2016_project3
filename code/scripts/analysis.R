# This script represents our analysis section of the report. This is where we will draw for functions
# for the Shiny app and for the results section of the report

# Loading Data FIles
aux_data = read.csv("../../data/aux-data.csv")
subset_dat = read.csv("../../data/subset-data.csv")

#Ridge Rankings
ridge_black = read.csv("../../data/ranked-ridge-black.csv")
ridge_hisp = read.csv("../../data/ranked-ridge-hispanic.csv")


# Shiny App main function and some tests to check that it works

# Status of schools  
inclusive = c(10,11)
selective = c(12,13)
highly_selective = c(14,15)

# This function takes a minority gorup a state and category of the four year university
# It returns a ranked data frame of what schools we are suggesting to target

by_state = function(group, state, status) {
  four_year = which(as.numeric(subset_dat$CCUGPROF) == status[1] 
                    | as.numeric(subset_dat$CCUGPROF) == status[2])
  # added from aux
  st = which(aux_data$states == state)
  four_year_st = intersect(four_year, st)
  four_year_st = intersect(group$index, four_year_st)
  temp_dat = subset(group, subset = index %in% four_year_st[1:10] & resid > 0)
  # added form aux
  names = aux_data$school_name[temp_dat$index]
  rows = temp_dat$index
  # added from aux
  total_pop = aux_data$total_pop[rows]
  total_grad = aux_data$total_grad[rows]
  output = cbind(names, subset_dat[rows,], total_grad, total_pop)
  final.output = data.frame(Names = output$names, 
                            African_American_Grad_Rate = output$C150_4_BLACK, 
                            Hispanic_Grad_Rate = output$C150_4_HISP, 
                           Aggragate_Grad_Rate = output$total_grad, 
                           Median_Earnings = output$MD_EARN_WNE_P8, 
                           Median_Debt = output$DEBT_MDN)
  
  return(final.output)

}

save(by_state, inclusive, selective, highly_selective, file = "../../data/analysis-script.RData")



# Turns Factors into numeric vectors
make_numeric = function(vector) {
  as.numeric(levels(vector))[vector]
}

# Making price into a single vector
net_price = subset_dat$NPT4_PUB
net_price_priv = subset_dat$NPT4_PRIV
net_price[is.na(net_price)] = net_price_priv[is.na(net_price)]

# Data Frame for African Americans in which schools have over 3% Afican American Student Body
# This Data Frame is for our Reuslts section in which we will isolate 

index = ridge_hisp$X
index = subset_dat[index,]
index = index[index$UGDS_HISP > 0.03,]

over3_hisp = as.data.frame(cbind(aux_data[index$X,c(2,3,4)], subset_dat[index$X,], 
                                  net_price = net_price[index$X]))

over3_hisp = over3_hisp[over3_hisp$CCUGPROF == 15 | over3_hisp$CCUGPROF == 14,
                          c("school_name", "ADM_RATE", "total_grad", "C150_4_HISP", "UGDS_HISP", "total_pop",
                            "net_price", "DEBT_MDN", "MD_EARN_WNE_P8")]

# Changing factors vectors into numeric
over3_hisp$total_grad = make_numeric(over3_hisp$total_grad)
over3_hisp$total_pop = make_numeric(over3_hisp$total_pop)
over3_hisp$DEBT_MDN = make_numeric(over3_hisp$DEBT_MDN)
over3_hisp$MD_EARN_WNE_P8 = make_numeric(over3_hisp$MD_EARN_WNE_P8)


# Data Frame for African Americans in which schools have over 3% Afican American Student Body
# This Data Frame is for our Reuslts section in which we will isolate 


index = ridge_black$X
index = subset_dat[index,]
index = index[index$UGDS_BLACK > 0.03,]

over3_black = as.data.frame(cbind(aux_data[index$X,c(2,3,4)], subset_dat[index$X,], 
                             net_price = net_price[index$X]))


over3_black = over3_black[over3_black$CCUGPROF == 15 | over3_black$CCUGPROF == 14,
                c("school_name", "ADM_RATE", "total_grad", "C150_4_BLACK", "UGDS_BLACK", "total_pop",
                  "net_price", "DEBT_MDN", "MD_EARN_WNE_P8")]
over3_black$total_grad = make_numeric(over3_black$total_grad)
over3_black$total_pop = make_numeric(over3_black$total_pop)
over3_black$DEBT_MDN = make_numeric(over3_black$DEBT_MDN)
over3_black$MD_EARN_WNE_P8 = make_numeric(over3_black$MD_EARN_WNE_P8)

save(over3_hisp, over3_black, file = "../../data/results-tables.RData")

