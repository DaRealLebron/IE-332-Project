#installing libraries
#install.packages("quantmod")
library("quantmod")
ticker <- c("AVY", "WHR", "MTD", "ECL", "BXP", "HSY", "DUK", "CTVA", "ZTS", "DAL", "MLM",
            "ALGN", "NEM", "PARA", "CEG", "EQR", "COST", "AIZ", "SEE","PKG")
#Loop through symbols, fetch prices, and store in myList

num_analyze <- read.csv("C:\\Users\\chase\\OneDrive\\Desktop\\A4\\AAPL_analyst_rec.csv")

#establish numerical values for each character input
num_analyze[num_analyze==''] <- 0
num_analyze[num_analyze=='Sell'] <- 1
num_analyze[num_analyze=='Underperform'] <- 1
num_analyze[num_analyze=='Underweight'] <- 1
num_analyze[num_analyze=='Reduce'] <- 1
num_analyze[num_analyze=='Negative'] <- 1
num_analyze[num_analyze=='Neutral'] <- 2
num_analyze[num_analyze=='Peer Perform'] <- 2
num_analyze[num_analyze=='Perform'] <- 2
num_analyze[num_analyze=='Market Perform'] <- 2
num_analyze[num_analyze=='Hold'] <- 2
num_analyze[num_analyze=='Sector Weight'] <- 2
num_analyze[num_analyze=='Sector Perform'] <- 2
num_analyze[num_analyze=='Equal-Weight'] <- 2
num_analyze[num_analyze=='Equal-weight'] <- 2
num_analyze[num_analyze=='Market Outperform'] <- 3
num_analyze[num_analyze=='Long-Term Buy'] <- 3
num_analyze[num_analyze=='Long-Term buy'] <- 3
num_analyze[num_analyze=='Long-term Buy'] <- 3
num_analyze[num_analyze=='Overweight'] <- 4
num_analyze[num_analyze=='Outperform'] <- 4
num_analyze[num_analyze=='Buy'] <- 4
num_analyze[num_analyze=='Accumulate'] <- 4
num_analyze[num_analyze=='Positive'] <- 4
num_analyze[num_analyze=='Sector Outperform'] <- 4
num_analyze[num_analyze=='Strong Buy'] <- 5

#numerical values for "actions" column
num_analyze[num_analyze=='maintains'] <- 3
num_analyze[num_analyze=='initiated'] <- 1
num_analyze[num_analyze=='reduce'] <- 1
num_analyze[num_analyze=='accumulate'] <- 4
num_analyze[num_analyze=='upgrade'] <- 5
num_analyze[num_analyze=='reiterates'] <- 4
num_analyze[num_analyze=='downgrade'] <- 2

#adding 2 new columns based on numerical values
num_analyze$rec_change <- as.numeric(num_analyze$toGrade) - as.numeric(num_analyze$fromGrade)
num_analyze$rec_score <- as.numeric(num_analyze$rec_change) * as.numeric(num_analyze$action)

rec_score <- num_analyze[,7]


#Note: "NA introduced by coercion" warning message does not effect the table



#Part 3
getSymbols("^GSPC", from = "2012-01-01", to = "2022-10-21", periodicity = "daily")
getSymbols("^HUI", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^XOI", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^DRG", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^BTK", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^PSE", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^xng", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^xal", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^xtc", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^nwx", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^SP500-40", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^SP500-20", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^SP500-30", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^SP500-50", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^SP500-60", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^buk100P", from="2012-01-01", to="2022-10-21", periodicity = "daily") 
getSymbols("^rut", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^vix", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^gdaxi", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^fchi", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^stoxx50e", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^n100", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^bfx", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("IMOEX.ME", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^N225", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^hsi", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("000001.SS", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("399001.SZ", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^STI", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^AXJO", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^JKSE", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^KS11", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^TWII", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^BVSP", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^MXX", from="2012-01-01", to="2022-10-21", periodicity = "daily")
getSymbols("^hko", from="2012-01-01", to="2022-10-21", periodicity = "daily")

#part ii

GSPC_adjust <- GSPC[,6]
HUI_adjust <- HUI[,6]
XOI_adjust <- XOI[,6]
DRG_adjust <- DRG[,6]
BTK_adjust <- BTK[,6]
PSE_adjust <- PSE[,6]
xng_adjust <- XNG[,6]
xal_adjust <- XAL[,6]
xtc_adjust <- XTC[,6]
nwx_adjust <- NWX[,6]
SP40_adjust <- `SP500-40`[,6]
SP20_adjust <- `SP500-20`[,6]
SP30_adjust <- `SP500-30`[,6]
SP50_adjust <- `SP500-50`[,6]
SP60_adjust <- `SP500-60`[,6]
BUK100P_adjust <- BUK100P[,6]
RUT_adjust <- RUT[,6]
VIX_adjust <- VIX[,6]
GDAXI_adjust <- GDAXI[,6]
FCHI_adjust <- FCHI[,6]
STOX_adjust <- STOXX50E[,6]
N100_adjust <- N100[,6]
BFX_adjust <- BFX[,6]
IMOEX_adjust <- IMOEX.ME[,6]
N225_adjust <- N225[,6]
HSI_adjust <- HSI[,6]
SS_adjust <- `000001.SS`[,6]
SZ_adjust <- `399001.SZ`[,6]
STI_adjust <- STI[,6]
AXJO_adjust <- AXJO[,6]
JKSE_adjust <- JKSE[,6]
KS11_adjust <- KS11[,6]
TWII_adjust <- TWII[,6]
BVSP_adjust <- BVSP[,6]
MXX_adjust <- MXX[,6]
HKO_adjust <- HKO[,6]

adjust <- merge(GSPC_adjust, HUI_adjust, XOI_adjust, DRG_adjust, BTK_adjust, PSE_adjust, xng_adjust, xal_adjust, 
                xtc_adjust, nwx_adjust, SP40_adjust, SP20_adjust, SP30_adjust, SP50_adjust, SP60_adjust, 
                BUK100P_adjust, RUT_adjust, VIX_adjust, GDAXI_adjust, FCHI_adjust, STOX_adjust, N100_adjust,
                BFX_adjust, IMOEX_adjust, N225_adjust, HSI_adjust, SS_adjust, SZ_adjust, STI_adjust, STI_adjust, 
                AXJO_adjust, JKSE_adjust, KS11_adjust, TWII_adjust, BVSP_adjust, MXX_adjust, HKO_adjust)
final_a <- na.omit(adjust)
principal_components <- prcomp(final_a, TRUE)




## Beginning of Part D





#Beginning of i
aapl_ret <- getSymbols("AAPL",from = "2012-01-01",to ="2022-10-21", auto.assign =
                                                    FALSE, periodicity = "daily")


#aapl_ret is everything getSymbols gets from AAPL
#full_aapl_ret is adjusted

full_aapl_ret <- aapl_ret[,6]

#full combined is principal components being combined with the rec score

full_combined = data.frame(Principal_Components = c((principal_components[["sdev"]]), rep(NA, length(rec_score) - length(principal_components[["sdev"]]))), rec_score = rec_score)

#install.packages("caret")
library("caret")


#X_sub and R_sub being found from previous data, giving us our neural network values

x_sub <- full_combined[(length(full_combined[["rec_score"]]) - 200) : (length(full_combined[["rec_score"]])), "rec_score"]

x_sub <- data.frame(x_sub = x_sub)



r_sub <- (full_aapl_ret[(length(full_aapl_ret) - 200) : length(full_aapl_ret)])

r_sub <- data.frame(r_sub = r_sub)

ctrl_aapl <- trainControl(method = "cv",
                          number = 5,
                          na.omit,
                          )

#actual neural network model utilizing the x subset and r subset
#following a repeatedcv method and five fold cross validation

n_n_model <- train(x_sub, as.numeric(unlist(r_sub)), 
                   method = "nnet",
                   trControl = ctrl_aapl
                   
                   )

n_n_model









