#libraries
library(haven)
library(ggplot2)
library(openintro)
library(dplyr)
library(knitr)
library(mFilter)
library(tidyverse)
library(lubridate)
library(zoo)
library(cowplot)
library(kableExtra)

#var packages
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(imputeTS)
library(plotly)

setwd("~/research_methods_project")

# loading data
FF <- read.csv('FEDFUNDS.csv')
M2 <- read.csv('M2SL.csv')
CPI <- read.csv('CPIAUCSL.csv')
GDP <- read.csv('GDP.csv')
WILL <- read.csv('W5000.csv')





WILL$DATE <- as.POSIXct(strptime(as.character(WILL$Date), "%Y-%m-%d"))
M2$DATE <- as.POSIXct(strptime(as.character(M2$DATE), "%Y-%m-%d"))
CPI$DATE <- as.POSIXct(strptime(as.character(CPI$DATE), "%Y-%m-%d"))
FF$DATE <- as.POSIXct(strptime(as.character(FF$DATE), "%Y-%m-%d"))
GDP$DATE <- as.POSIXct(strptime(as.character(GDP$DATE), "%Y-%m-%d"))

WILL <- WILL[-c(1,3:7)]
WILL <- WILL[,c(2,1)]


# creating new df of merged variables 
df_list <- list(FF, M2, CPI, GDP, WILL)
df <- df_list %>%
  reduce(full_join, by='DATE')


# cleaning up data to include only data from 02/01/1989 to 01/01/2022
new_df <- df[-c(1:417,814:907),] 


df_renamed <- new_df %>% 
  rename(
    M2 = M2SL,
    CPI = CPIAUCSL,
    W5000 = Open
  )

# interpolating GDP monthly NAs with correct value for the quarter (will also substitute missing values in other columns with previous value)
df_intp <- na.locf(df_renamed, fromLast = TRUE)



df_intp$W5000 <- as.numeric(df_intp$W5000) 

# loading df with sentiment included
df_sent <- read.csv('spliced.csv')
tail(df_sent, n=10)
head(df_sent, n=10)

# stripping chr date and converting to posix
df_sent$DATE <- ymd(df_sent$DATE)
glimpse(df_sent)


# preparing descriptive statistics 
WILL_l <- df_sent %>%
  dplyr::select(W5000) %>%
  summarize(min = min(W5000),
            max = max(W5000),
            mean = mean(W5000),
            sd = sd(W5000),
            n_obs = n()
  )

FEDFUNDS_l <- df_sent %>%
  dplyr::select(FEDFUNDS) %>%
  summarize(min = min(FEDFUNDS),
            max = max(FEDFUNDS),
            mean = mean(FEDFUNDS),
            sd = sd(FEDFUNDS),
            n_obs = n()
  )

M2_l <- df_sent %>%
  dplyr::select(M2) %>%
  summarize(min = min(M2),
            max = max(M2),
            mean = mean(M2),
            sd = sd(M2),
            n_obs = n()
  )

CPI_l <- df_sent %>%
  dplyr::select(CPI) %>%
  summarize(min = min(CPI),
            max = max(CPI),
            mean = mean(CPI),
            sd = sd(CPI),
            n_obs = n()
  )

GDP_l <- df_sent %>%
  dplyr::select(GDP) %>%
  summarize(min = min(GDP),
            max = max(GDP),
            mean = mean(GDP),
            sd = sd(GDP),
            n_obs = n()
  )

Bert_l <- df_sent %>%
  dplyr::select(Bert_Sent) %>%
  summarize(min = min(Bert_Sent),
            max = max(Bert_Sent),
            mean = mean(Bert_Sent),
            sd = sd(Bert_Sent),
            n_obs = n()
  )

Blob_l <- df_sent %>%
  dplyr::select(Blob_Sent) %>%
  summarize(min = min(Blob_Sent),
            max = max(Blob_Sent),
            mean = mean(Blob_Sent),
            sd = sd(Blob_Sent),
            n_obs = n()
  )

# converting to dataframes
WILL_df <- as.data.frame(WILL_l)
GDP_df <- as.data.frame(GDP_l)
CPI_df <- as.data.frame(CPI_l)
M2_df <- as.data.frame(M2_l)
FEDFUNDS_df <- as.data.frame(FEDFUNDS_l)
Bert_l <- as.data.frame(Bert_l)
Blob_l <- as.data.frame(Blob_l)

# binding descriptive statistic dataframes 
binded <-rbind.data.frame(WILL_df, GDP_df, CPI_df, M2_df,FEDFUNDS_df, Bert_l, Blob_l)


# adding proper labels and rearranging columns
labels <- c('W5000', 'GDP', 'CPI', 'M2', 'FF', "Bert_l","Blob_l")
binded$variable <- labels

binded <- binded[,c(6,1,2,3,4,5)]
binded
# generating kable table 
table <- kable(
  binded, 'simple',
  col.names = c("Variable", "Min", "Max", "Mean", "Standard Deviation", "n_obs"),
  digits = 2,
  caption = "Descriptive Statistics"
)   
table


# preparing box plots of un-adjusted data
figFED <- plot_ly(y = df_sent$FEDFUNDS, type = "box", quartilemethod="exclusive", name='FFR') # or "inclusive", or "linear" by default
figGDP <- plot_ly(y = df_sent$GDP, type = "box", quartilemethod="exclusive", name='GDP') # or "inclusive", or "linear" by default
figM2 <- plot_ly(y = df_sent$M2, type = "box", quartilemethod="exclusive", name='M2') # or "inclusive", or "linear" by default
figCPI <- plot_ly(y = df_sent$CPI, type = "box", quartilemethod="exclusive", name='CPI') # or "inclusive", or "linear" by default
figW5000 <- plot_ly(y = df_sent$W5000, type = "box", quartilemethod="exclusive", name='W5000') # or "inclusive", or "linear" by default
figBert <- plot_ly(y = df_sent$Bert_Sent, type = "box", quartilemethod="exclusive", name='Bert_Sent') # or "inclusive", or "linear" by default
figBlob <- plot_ly(y = df_sent$Blob_Sent, type = "box", quartilemethod="exclusive", name='Blob_Sent') # or "inclusive", or "linear" by default

figFED
figGDP
figM2
figCPI
figW5000
figBert
figBlob

fig_grid <- subplot(figFED, figGDP, figM2, figCPI, figW5000, figBert, figBlob) %>%
  layout(title = 'Figure 1: Box Plots')

fig_grid


W5000_box <- ggplot(df_sent, aes(x=FEDFUNDS, y = W5000)) + geom_point()
FEDFUNDS_box <- ggplot(df_sent, aes(x=FEDFUNDS, y=FEDFUNDS)) + geom_point()
M2_box <- ggplot(df_sent, aes(x=FEDFUNDS, y=M2)) + geom_point() 
CPI_box <- ggplot(df_sent, aes(x=FEDFUNDS, y=CPI)) + geom_point() 
GDP_box <- ggplot(df_sent, aes(x=FEDFUNDS, y=GDP)) + geom_point()
Bert_box <- ggplot(df_sent, aes(x=FEDFUNDS, y=Bert_Sent)) + geom_point() 
Blob_box <- ggplot(df_sent, aes(x=FEDFUNDS, y=Blob_Sent)) + geom_point()

W5000_box
M2_box
FEDFUNDS_box
GDP_box
Bert_box
Blob_box

box_title <- ggdraw() + draw_label("Figure 1: Box Plots", fontface='bold')
plot_grid(box_title, W5000_box, FEDFUNDS_box, M2_box, CPI_box)
plot_grid(box_title, GDP_box, Bert_box, Blob_box)

xtable

# differencing data for line plot comparisons 

diff <- df_sent %>%
  mutate(W5000 = c(NA, diff(log(W5000))),
         GDP = c(NA, diff(log(GDP))),
         M2 = c(NA, diff(log((M2)))),
         CPI = c(NA, diff(log(CPI))))
diff <- diff[-1,]

fig_line_W5000 <- plot_ly(diff, x = ~DATE, y = ~W5000, name = 'W5000', type = 'scatter', mode = 'lines',
               line = list(color = 'rgb(205, 12, 24)', width = 1)) 
fig_line_CPI <- plot_ly(diff, x = ~DATE, y = ~CPI, name = 'CPI', type = 'scatter', mode = 'lines',
                    line = list(color = 'rgb(67, 67, 67)', width = 1)) 
fig_line_M2 <- plot_ly(diff, x = ~DATE, y = ~M2, name = 'M2', type = 'scatter', mode = 'lines',
                    line = list(color = 'rgb(152,0,0)', width = 1)) 
fig_line_GDP <- plot_ly(diff, x = ~DATE, y = ~GDP, name = 'GDP', type = 'scatter', mode = 'lines',
                    line = list(color = 'rgb(189, 100, 0)', width = 1)) 
fig_line_FEDFUNDS <- plot_ly(diff, x = ~DATE, y = ~FEDFUNDS, name = 'FFR', type = 'scatter', mode = 'lines',
                    line = list(color = 'rgb(189, 100, 40)', width = 1)) 
fig_line_BERT <- plot_ly(diff, x = ~DATE, y = ~Bert_Sent, name = 'Bert Sent', type = 'scatter', mode = 'lines',
                    line = list(color = 'rgb(0, 122, 0)', width = 1)) 
fig_line_BLOB <- plot_ly(diff, x = ~DATE, y = ~Blob_Sent, name = 'Blob Sent', type = 'scatter', mode = 'lines',
                    line = list(color = 'rgb(0, 0, 150)', width = 1)) 


annotations = list( 
  list( 
    x = 0.2,  
    y = 1.0,  
    text = "Plot 1",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.8,  
    y = 1,  
    text = "Plot 2",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.2,  
    y = 0.45,  
    text = "Plot 3",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),
  list( 
    x = 0.8,  
    y = 0.45,  
    text = "Plot 4",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),
  list( 
    x = 0.8,  
    y = 0.45,  
    text = "Plot 4",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  list( 
    x = 0.8,  
    y = 0.45,  
    text = "Plot 4",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ))



fig_grid_line <- subplot(fig_line_W5000, fig_line_CPI, fig_line_M2, fig_line_GDP, fig_line_BERT, fig_line_BLOB, nrows=2) %>%
  layout(title = list('Figure 1: Box Plots'),
    plot_bgcolor='#e5ecf6',
    xaxis = list(
        zerolinecolor = '#ffff', 
        zerolinewidth = 2, 
        gridcolor = 'ffff'),
    yaxis = list(
      zerolinecolor = '#ffff', 
      zerolinewidth = 2, 
      gridcolor = 'ffff')
  )

fig_grid_line <- fig_grid_line %>% layout(annotations=annotations)
fig_grid_line


fig_line_FEDFUNDS



# preparing ggplots for cowplot
W5000_line <- ggplot(diff, aes(x=DATE, y = W5000)) + geom_line()
FEDFUNDS_line <- ggplot(df_sent, aes(x=DATE, y=FEDFUNDS)) + geom_line()
M2_line <- ggplot(diff, aes(x=DATE, y=M2)) + geom_line() 
CPI_line <- ggplot(diff, aes(x=DATE, y=CPI)) + geom_line() 
GDP_line <- ggplot(diff, aes(x=DATE, y=GDP)) + geom_line()

# generating plot collage 
title <- ggdraw() + draw_label("Adjusted Line Plots of Variables", fontface='bold')
top_row <- plot_grid(M2_line, CPI_line, GDP_line, ncol=3)
bottom_row <- plot_grid(W5000_line, FEDFUNDS_line, ncol=2)
plot_grid(title, top_row, bottom_row, 
          ncol = 1,rel_heights = c(0.1,1, 1))

# run linear regression on the data with W5000 as the dependent variable
lmW5000_4 = lm(d_W5000 ~ FEDFUNDS + d_GDP + d_M2 + d_CPI, data = diff)
summary(lmW5000_4)

# taking log of original data (except FFR) and re-running regression
# maybe then difference ... more than likely 

df_transformed <- df_intp %>%
  mutate(diff_log_M2 = c(NA, diff(log(M2))),
  diff_log_GDP = c(NA, diff(log(GDP))),
  diff_log_CPI = c(NA, diff(log(CPI))),
  diff_log_W5000 = c(NA, diff(log(W5000))))

df_sent <- df_sent[-c(814:907),] 

# transforming data frame with sentiment data
df__sent_transformed <- df_sent %>%
  mutate(diff_log_M2 = c(NA, diff(log(M2))),
         diff_log_GDP = c(NA, diff(log(GDP))),
         diff_log_CPI = c(NA, diff(log(CPI))),
         diff_log_W5000 = c(NA, diff(log(W5000))))

# remove extra columns
df_transformed <- df_transformed[-c(3:6)]
df_transformed <- df_transformed[-1,]


stock_fig <- ggplot(df_transformed, aes(x=FEDFUNDS, y =diff_log_W5000, na.rm=TRUE)) + geom_point()
stock_fig




















# let's try a VAR analysis and see what we can find 

head(df_VAR, n=10)


# creating TS objects
FF_TS <- ts(df_VAR$FEDFUNDS, start = c(1989,4,1), frequency = 12)
M2_TS <- ts(df_VAR$diff_log_M2, start = c(1989,4,1), frequency = 12)
CPI_TS <- ts(df_VAR$diff_log_CPI, start = c(1989,4,1), frequency = 12)
GDP_TS <- ts(df_VAR$diff_log_GDP, start = c(1989,4,1), frequency = 12)
W5000_TS <- ts(df_VAR$diff_log_W5000, start = c(1989,4,1), frequency = 12)

sys1 <- cbind(FF_TS, M2_TS, CPI_TS, GDP_TS, W5000_TS)
colnames(sys1) <- cbind("M2","CPI","GDP", "FF")

# selecting lag order with VARselect()
lagselect <- VARselect(sys1, lag.max = 20, type = "const")
lagselect$selection

# we will use 15 lags.. time to fit the model
model <- VAR(sys1, p = 15, type = "const", season = NULL, exog = NULL) 
summary(model)

# testing for serial correlated errors by computing Portmaneteau 
autocor <- serial.test(model, lags.pt = 15, type = "PT.asymptotic")
autocor
# we do see autocorrelation among residuals 

# testing for heteroskedasticity 
ARCH <- arch.test(model, lags.multi = 15, multivariate.only = TRUE)
ARCH
# can reject the presence of het. w/ significant p value 

# testing for normality (soft-req)
norm <- normality.test(model, multivariate.only = TRUE)
norm

# testing for structural breaks w a plot of the sum of recursive residuals 
stab <- stability(model, type = "OLS-CUSUM")
# change fig margins to fit plot then plot
par(mar=c(1,1,1,1))
plot(stab)
# if the data extends outside the red critical regions this indicates the presence of
# a structural break. None observed here. 

# granger causality tests to check for causal relationships between variables 

gFF <- causality(model, cause = "FF_TS")
gFF

gM2 <- causality(model, cause = "M2_TS")
gM2

gCPI <- causality(model, cause = "CPI_TS")
gCPI

gGDP <- causality(model, cause = "GDP_TS")
gGDP

gW5000 <- causality(model, cause = "W5000_TS")
gW5000

# now let's take a look at the impulse response functions 
irfFF <- irf(model, impulse = "FF_TS", response = "FF_TS", n.ahead = 20, boot = TRUE)
plot(irfFF, ylab = "FF_TS", main = "FF's shock to FF")

irfM2 <- irf(model, impulse = "FF_TS", response = "M2_TS", n.ahead = 20, boot = TRUE)
plot(irfM2, ylab = "M2_TS", main = "FF's shock to M2")

irfCPI <- irf(model, impulse = "FF_TS", response = "CPI_TS", n.ahead = 20, boot = TRUE)
plot(irfCPI, ylab = "CPI", main = "FF's shock to CPI")

irfGDP <- irf(model, impulse = "FF_TS", response = "GDP_TS", n.ahead = 20, boot = TRUE)
plot(irfGDP, ylab = "GDP", main = "FF's shock to GDP")

irfW5000 <- irf(model, impulse = "FF_TS", response = "W5000_TS", n.ahead = 20, boot = TRUE)
plot(irfW5000, ylab = "W5000", main = "FF's shock to W5000")

# forecast error variance decomposition for FF
ffVD <- fevd(model, n.ahead = 15)
ffVD
plot(ffVD)

# now generate forecasts
forecast <- predict(model, n.ahead = 24, ci = 0.95)
fanchart(forecast, names = "FF_TS", main = "Forecast for FF", xlab = "Horizon", ylab = "FF")
fanchart(forecast, names = "M2_TS", main = "Forecast for M1", xlab = "Horizon", ylab = "M2")
fanchart(forecast, names = "CPI_TS", main = "Forecast for CPI", xlab = "Horizon", ylab = "CPI")
fanchart(forecast, names = "GDP_TS", main = "Forecast for GDP", xlab = "Horizon", ylab = "GDP")
fanchart(forecast, names = "W5000_TS", main = "Forecast for W5000", xlab = "Horizon", ylab = "GDP")

forecast

#

