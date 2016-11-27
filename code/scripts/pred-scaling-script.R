# Data Cleaning

dat <- read.csv("../../data/subset-data.csv")[,-1]

# Scaled Test Sets
pred_black = na.omit(data.frame(dat[,"UNTID", "UGDS_BLACK", "COMPL_RPY_5YR_RT",]))

# Helper function to turn factors into numerics
make_numeric = function(vector) {
  as.numeric(levels(vector))[vector]
}

# Making all necessary data numeric based
dat$COMPL_RPY_3YR_RT = make_numeric(dat$COMPL_RPY_3YR_RT)
dat$COMPL_RPY_5YR_RT = make_numeric(dat$COMPL_RPY_5YR_RT)
dat$COMPL_RPY_3YR_RT = make_numeric(dat$COMPL_RPY_3YR_RT)
dat$COMPL_RPY_7YR_RT = make_numeric(dat$COMPL_RPY_7YR_RT)
dat$DEBT_MDN = make_numeric(dat$DEBT_MDN)
dat$MD_EARN_WNE_P10 = make_numeric(dat$MD_EARN_WNE_P10)
dat$MD_EARN_WNE_P6 = make_numeric(dat$MD_EARN_WNE_P6)
dat$MD_EARN_WNE_P8 = make_numeric(dat$MD_EARN_WNE_P8)

# Make to total cost vector
dat$NPT4_PUB[is.na(dat$NPT4_PUB)] = dat$NPT4_PRIV[is.na(dat$NPT4_PUB)]

#make total grad
demo = dat[, c("UGDS_WHITE", "C150_4_WHITE", "C150_4_ASIAN", "UGDS_ASIAN", 
               "UGDS_BLACK", "C150_4_BLACK", "UGDS_HISP", "C150_4_HISP")]
demo[is.na(demo)] <- 0
grad_total = demo$UGDS_WHITE*demo$C150_4_WHITE + demo$C150_4_ASIAN*demo$UGDS_ASIAN + 
  demo$UGDS_BLACK*demo$C150_4_BLACK + demo$UGDS_HISP*demo$C150_4_HISP

#data to be scaled
preds = cbind(dat[,c("UGDS_BLACK", "UGDS_HISP", "COMPL_RPY_5YR_RT", "NPT4_PUB", 
                     "ADM_RATE", "RET_FT4", "DEBT_MDN", "PCTPELL", "MD_EARN_WNE_P10")], 
              grad_total, dat[,c("C150_4_BLACK", "C150_4_HISP")])

scaled_dat <- scale(preds, center = TRUE, scale = TRUE)

scaled_dat = as.data.frame(cbind(1:nrow(scaled_dat), dat$UNITID, scaled_dat))
colnames(scaled_dat)[1:2] = c("index", "UNITID")

write.csv(scaled_dat, "../../data/scaled-predictors.csv")