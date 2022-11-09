num_analyze <- read.csv("C:\\Users\\amitl\\OneDrive\\Documents\\Purdue\\IE332\\A4\\AAPL_analyst_rec.csv")

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

#Note: "NA introduced by coercion" warning message does not effect the table