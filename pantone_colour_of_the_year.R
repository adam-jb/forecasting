
# code to predict Pantone's next colour of the year, as per the question: 
# https://www.metaculus.com/questions/3420/what-will-be-pantones-color-of-the-year-for-2021/

library(tidyverse)
library(ranger)
library(caret)



setwd("~/Downloads")
x <- read_csv("training_test_pantone_raw - Sheet1.csv") %>% select(-Year)

table(x$Bin) # same bin has never been found in past year so say it won't be bin 2

# overriding distances to bin with the bins of past years: rf does worse with this override than without it
# x <- x %>% mutate(c1 = Bin - c1,
#                   c2 = Bin - c2,
#                   c3 = Bin - c3,
#                   c4 = Bin - c4,
#                   c5 = Bin - c5)


idx <- createDataPartition(x$Bin, p = 0.6, times = 1)

train <- x[idx$Resample1, ]
test <- x[-idx$Resample1, ]

rf <- ranger(Bin ~ ., data = train, probability = T)

preds <- predict(rf, test)$predictions

# drop prediction for previous year
for (i in 1:nrow(preds)) {
  col_id <- unlist(test[i, 1]) - unlist(test[i, 2])
  preds[i, col_id] <- 0
}

# From inspection: ML very eratic (as one might expect with this little data)



### stats method
table(abs(x$c1)) / nrow(x)   # 2 years before looks good
table(abs(x$c2)) / nrow(x)

table(abs(x$c3)) / nrow(x)   # signal appears to break down after 2 years


# so base prediction on previous 2 years: should it matter if someone is on one end of the spectrum?
# naive method below:

# what was last years, and therefore what is the expected value pf this year?
# what was year before, and therefoore what is expected value of this year?
freq_table <- table(abs(x$c1)) / nrow(x)
find_From_last_year <- function(in_val, freq_table) {
  out <- rep(0, 6)
  for (i in 1:6){
    ix <- abs(in_val - i)
    try(out[i] <- freq_table[toString(ix)], silent = T)
  }
  out[is.na(out)] <- 0
  return(out / sum(out))
}

last_yr <- 2  # what was it year?
yr_before <- 5

yr1 <- find_From_last_year(last_yr, freq_table)

freq_table <- table(abs(x$c2)) / nrow(x)
yr2 <- find_From_last_year(yr_before, freq_table)

base_rate <- table(x$Bin) / sum(table(x$Bin))

# combining base rate and last 2 years to get a prediction
poss <- (yr1 * yr2 * base_rate) / sum(yr1 * yr2 * base_rate)
poss

## gives following probabilities to colour bins for 2021:
#
#         1          2          3          4          5 
#0.08955224 0.00000000 0.14925373 0.44776119 0.13432836 
#         6 
#0.17910448 







