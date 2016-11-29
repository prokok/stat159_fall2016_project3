# Loading Data FIles
aux_data = read.csv("../../data/aux-data.csv")
subset_dat = read.csv("../../data/subset-data.csv")

#OLS rankings 
ols_black = read.csv("../../data/ranked-ols-black.csv")
ols_hisp = read.csv("../../data/ranked-ols-hisp.csv")

#Ridge Rankings
ridge_black = read.csv("../../data/ranked-ridge-black.csv")
ridge_hisp = read.csv("../../data/ranked-ridge-hispanic.csv")


# Status is 
inclusive = c(10,11)
selective = c(12,13)
highly_selective = c(14,15)

# State is 
CA = 6
NY = 36


#####
# I need the school names breh

# This function takes a minority gorup a state ategory of the four year university

by_state = function(group, state, status) {
  four_year = which(as.numeric(subset_dat$CCUGPROF) == status[1] 
                    | as.numeric(subset_dat$CCUGPROF) == status[2])
  # added with aux
  st = which(aux_data$states == state)
  four_year_st = intersect(four_year, st)
  four_year_st = intersect(group$index, four_year_st)
  temp_dat = subset(group, subset = index %in% four_year_st[1:10] & resid > 0)
  # need to add
  names = aux_data$school_name[temp_dat$index]
  rows = temp_dat$index
  # need to add
  total_grad = aux_data$total_grad[rows]
  cbind(names, subset_dat[rows,], total_grad)

}

by_state(ridge_hisp, CA, selective)






