# Alenmathew.SP500-Prediction
# Load library need
library(tidyquant)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(GGally)
library(readxl)
library(tidyverse)
library(purrr)
library(factoextra)
library(MASS)
library(mlr)
library(tidygraph)
# 1. Load data of SP&500
# https://fred.stlouisfed.org/series/SP500
DF_initial <- read_excel("SP500.xlsx", sheet = "Sheet1")
DF <- data.frame(Date= DF_initial$Date,SP500 = DF_initial$`Close*`)
#head(DF)
#View(DF)
# Conver values from "." to NA and convert column SP500 to numeric
DF$SP500 <- ifelse(DF$SP500 == ".", NA, DF$SP500)
DF$SP500 <- as.numeric(DF$SP500)
#Transform to the quarter format
fred_SP500_quarterly <- DF %>%
mutate(quarter = as.yearqtr(as.Date(Date, format = "%Y-%m-%d"))) %>%
group_by(quarter) %>%
summarize(mean_SP500 = mean(SP500, na.rm = TRUE)) %>%ungroup()
#Staring at 1997 for SP500 data set
fred_SP500_quarterly_97 <- fred_SP500_quarterly[-c(1:28),]
#since april does not have much data deleting it
fred_SP500_quarterly_97 <- fred_SP500_quarterly_97[-106,]
#View(fred_SP500_quarterly)
#head(fred_SP500_quarterly)
line_plot <- ggplot(fred_SP500_quarterly_97, aes(x = factor(quarter), y = mean_SP500, group = 1)) +
geom_line(color = "steelblue", size = 1.5) +
theme_minimal() +
labs(x = "Quarters", y = "Median of S&P 500", title = "Media per quarter S&P 500") +
scale_x_discrete(labels = as.character(fred_SP500_quarterly_97$quarter)) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
line_plot
# 2. Define FRED indicators to use
fred_symbols <- c("A191RL1Q225SBEA", # Real Gross Domestic Produc | https://fred.stlouisfed.org/series/A191RL1Q225SBEA
"PERMIT", # New Privately-Owned Housing Units Authorized in Permit-Issuing Places: Total Units | https://fred.stlouisfed.org/series/PERMIT
"CSUSHPINSA", # S&P/Case-Shiller U.S. National Home Price Index | https://fred.stlouisfed.org/series/CSUSHPINSA
"UMCSENT", # University of Michigan: Consumer Sentiment | https://fred.stlouisfed.org/series/UMCSENT
"UNRATE", # Unemployment Rate | https://fred.stlouisfed.org/series/UNRATE
"CPALTT01USM657N", # Consumer Price Index: Total All Items for the United States | https://fred.stlouisfed.org/series/CPALTT01USM657N
"PPIACO", # Producer Price Index by Commodity: All Commodities | https://fred.stlouisfed.org/series/PPIACO
"ANFCI", # Chicago Fed Adjusted National Financial Conditions Index | https://fred.stlouisfed.org/series/ANFCI
"T10Y2Y", # 0-Year Treasury Constant Maturity Minus 2-Year Treasury Constant Maturity | https://fred.stlouisfed.org/series/T10Y2Y
"BAMLH0A0HYM2EY", # ICE BofA US High Yield Index Effective Yield | https://fred.stlouisfed.org/series/BAMLH0A0HYM2EY
"M2V", # Velocity of M2 Money Stock | https://fred.stlouisfed.org/series/M2V
"QBPBSTASSCUSTRSC", # Balance Sheet: Total Assets: Securities: U.S. Treasury Securities | https://fred.stlouisfed.org/series/QBPBSTASSCUSTRSC
"QBPBSTAS") # Balance Sheet: Total Assets | https://fred.stlouisfed.org/series/QBPBSTAS
# 3. Download FRED info
fred_data <- tq_get(fred_symbols, get = "economic.data", from = "1997-01-01")
# Convert info in quarterly performance metric using average (mean)
fred_data$date <- as.Date(fred_data$date)
#FRED indicators in quarterly format
fred_data_quarterly <- fred_data %>%
mutate(quarter = as.yearqtr(as.Date(date, format = "%Y-%m-%d"))) %>%
group_by(symbol, quarter) %>%
summarize(price = mean(price, na.rm = TRUE)) %>%
ungroup()
#making fred data to wide data set
fred_data_wide <- fred_data_quarterly %>%
spread(symbol, price)
fred_data_wide <- as_tibble(fred_data_wide)
# because of april, remove last row
fred_data_wide <- fred_data_wide[-106,]
##function for finding percentage change
rate <- function(x,n){
( x-lag(x,n))/lag(x,n)*100
# percentage change of FRED indicators (13 variables)
percentage_change <- rate(fred_data_wide,1)
# percentage change of SP
sp_per_chang <- rate(fred_SP500_quarterly_97,1)
# Create a data frame with lagged percentage change variables 13 feature+quarter
lagged_per_ch <- data.frame(
percentage_change,
lag1 = lag(percentage_change, 1),
lag2 = lag(percentage_change, 2),
lag3 = lag(percentage_change, 3)
)
#removing quarter column
lagged_per_ch <- lagged_per_ch[,c(-1,-15,-29,-43)]
#removing na so basically data started at 1998
lagged_per_ch <- lagged_per_ch[c(-1,-2,-3,-4),]
# combine sp and fred
# for sp start at 1998 since when we lag we lost one year
sp_per_chang <- sp_per_chang[c(-1,-2,-3,-4),]
SP_quarter_perc <- cbind(sp_per_chang,lagged_per_ch)
SP_quarter_perc_tib <- as_tibble(SP_quarter_perc)
SP_quarter_perc_tib
#Cleaning the data by removing the rows NA (1 row from the bottom)
SP <- SP_quarter_perc_tib[1:100,]
SP
#performing PCA
SP_PCA <- select(SP, c(-mean_SP500,-quarter)) %>%prcomp(center = TRUE, scale = TRUE)
SP_PCA
summary(SP_PCA)
#variable loading for PCA
b<-map_dfc(1:19, ~SP_PCA$rotation[, .] * sqrt(SP_PCA$sdev ^ 2)[.])
order(abs(b$...5))
# PLOTTING PCA RESULT ----
pcaDat <- get_pca(SP_PCA)
pcaDat$coord
fviz_pca_biplot(SP_PCA, label = "var")
fviz_pca_var(SP_PCA, axes = c(3,4))
fviz_screeplot(SP_PCA, addlabels = TRUE, choice = "eigenvalue")
fviz_screeplot(SP_PCA, addlabels = TRUE, choice = "variance", ncomp = 19)
PCA1 = SP_PCA$x[, 1]
dim(fred_data_wide)
SP_PCA
#adding pca component of data set
PcaSP <- SP %>% mutate(PC1 = SP_PCA$x[, 1], PC2 = SP_PCA$x[, 2],
PC3 = SP_PCA$x[, 3],PC4 = SP_PCA$x[, 4],
PC5 = SP_PCA$x[, 5],PC6 = SP_PCA$x[, 6],
PC7 = SP_PCA$x[, 7],PC8 = SP_PCA$x[, 8],
PC9 = SP_PCA$x[, 9],PC10 = SP_PCA$x[, 10],
PC11 = SP_PCA$x[, 11],PC12 = SP_PCA$x[, 12],PC13 = SP_PCA$x[, 13],PC14 = SP_PCA$x[, 14],
PC15 = SP_PCA$x[, 15],PC16 = SP_PCA$x[, 16],PC17 = SP_PCA$x[, 17],PC18 = SP_PCA$x[, 18],PC19 = SP_PCA$x[, 19])
# Here the 'x' values are the original features expressed in terms of the principal components!
# Linear Model
#------------------------------------------------------------
# linear model with pca
model_lm <-lm(mean_SP500 ~ PC1+PC2+PC3+PC4+PC8+PC16+PC18,data=PcaSP)
summary(model_lm)
#put here current data to find relative change of next quarter
currentdata <- as.data.frame( SP[54,])
currentdata_pca <- predict(SP_PCA,currentdata)
k <- as.data.frame(currentdata_pca[,1:19])
tt <- t(k)
tta <- data.frame(tt)
predict(model_lm,tta)
#----------------------------------------------------
# Extract the loadings of the top first 4 principal components
loadings <- get_pca_var(SP_PCA)$contrib[, 1:4]
# Get the names of the 5 most influential variables for each component
top_vars <- apply(loadings, 2, function(x) names(x)[order(x, decreasing = TRUE)[1:5]])
# Create a bar plot of the top variables for each component
top_vars_df <- data.frame(component = rep(1:5, each = 4), variable = unlist(top_vars))
#untidy format for easy plot
titanicUntidy <- gather(top_vars_df, key = "variable", value = "component")
# Define a custom color palette with four colors
my_palette <- c("#1a53ff", "#22a7f0", "#a7d5ed", "palegreen1", "wheat","#1a53ff" , "#22a7f0", "#a7d5ed", "palegreen1"
, "wheat", "#1a53ff", "#22a7f0", "sandybrown", "red", "red4", "palegreen1", "wheat", "#1a53ff","red")
my_palette <- c("#004c6d", "#abffff", "#d1ffff", "#5383a1", "#7faac6","#004c6d" , "#abffff", "#d1ffff", "#5383a1"
, "#7faac6", "#004c6d", "#abffff", "#d1ffff", "#bee7eb", "#d1ffff", "#5383a1", "#7faac6", "#004c6d","#bee7eb")
ggplot(titanicUntidy, aes(x = variable, y = component, fill = component)) +
geom_bar(stat = 'identity') +
scale_fill_manual(values = my_palette) +
theme_classic() +
theme(axis.text.y = element_blank()) + # Remove y-axis text
labs(title = 'Top 5 Influential Variables of Top 4 Principal Components', x = 'Principle components', y = 'Variables')
# Logistic regression
#-------------------------------------------------
SP1 <- SP[,-1]
#creating a column to see if relative change of sp is bigger than 0
SP1$updown <- ifelse(SP$mean_SP500 >= 0, 1, 0)
SP2 <- SP1[,-1]
# CREATE TASK AND LEARNER, AND ATTEMPT TO TRAIN MODEL
new_sp <- as.data.frame(SP2)
new_sp$updown <- factor(new_sp$updown)
SPTask <- makeClassifTask(data = new_sp, target = "updown")
logReg <- makeLearner("classif.logreg", predict.type = "prob")
logRegModel <- train(logReg, SPTask)
#cv
kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50,
stratify = TRUE)
kFoldCV <- resample(learner = logReg, task = SPTask,
resampling = kFold, measures = list(mmce, acc))
kFoldCV$aggr
kFoldCV$measures.test
calculateConfusionMatrix(kFoldCV$pred, relative = TRUE)
# with logistic regression, our accuracy is 0.57
logRegModelData <- getLearnerModel(logRegModel)
coef(logRegModelData)
# # feature having negative correlation and are relavant
SP_all <- SP[, c("mean_SP500", "BAMLH0A0HYM2EY","QBPBSTAS","UNRATE")]
SP_all_untidy <- gather(SP_all, key = "Variable",
value = "Value", -mean_SP500)
cor_df_all <- SP_all_untidy %>%
group_by(Variable) %>%
summarize(cor = cor(Value, mean_SP500), .groups = "drop")
# Create scatterplot with correlation coefficients
ggplot(SP_all_untidy, aes(Value, mean_SP500)) +
facet_wrap(~ Variable, scale = "free_x", labeller = labeller(Variable = c("BAMLH0A0HYM2EY" = " ICE BofA US High Yield Index Effective Yield",
"QBPBSTAS" = "Balance Sheet: Total Assets" , "UNRATE" = "Unemployment Rate"))) +
geom_point() +
geom_smooth(method = "lm", col = "red") +
geom_text(data = cor_df_all, aes(label = paste0("cor = ", round(cor, 2)),
x = Inf, y = Inf), hjust = 1.2, vjust = 1.2, size = 4) +xlab("Value") + ylab("SP500") +
theme_bw()
# # feature having positive correlation and are relavant
SP_all <- SP[, c("mean_SP500", "CSUSHPINSA","M2V")]
SP_all_untidy <- gather(SP_all, key = "Variable",
value = "Value", -mean_SP500)
cor_df_all <- SP_all_untidy %>%
group_by(Variable) %>%
summarize(cor = cor(Value, mean_SP500), .groups = "drop")
# Create scatterplot with correlation coefficients
ggplot(SP_all_untidy, aes(Value, mean_SP500)) +
facet_wrap(~ Variable, scale = "free_x", labeller = labeller(Variable = c("CSUSHPINSA" = "S&P/Case-Shiller U.S. National Home Price Index", "M2V" = "Velocity of M2 Money Stock"))) +
geom_point() +
geom_smooth(method = "lm", col = "red") +
geom_text(data = cor_df_all, aes(label = paste0("cor = ", round(cor, 2)),
x = Inf, y = Inf), hjust = 1.2, vjust = 1.2, size = 4) + xlab("Value") + ylab("SP500") +
theme_bw()
