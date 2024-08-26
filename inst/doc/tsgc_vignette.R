## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.width = 7,
                      fig.height = 7)

library(tsgc)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggfortify)
library(ggforce)
library(magrittr)
library(zoo)
library(ggfortify)
library(latex2exp)
library(xts)
library(gridExtra)
library(here)
library(timetk)
theme_set(theme_economist_white(gray_bg = FALSE, base_size = 16))

## ----fig-1, include = F-------------------------------------------------------
# Load Gauteng data
data(gauteng, package = "tsgc")

# Calculate the 7 day centered moving average
Y <- gauteng
ma.cent.new.cases <- zoo::rollmean(diff(Y), 7, align = "center")
str(Y)

# Find the date at which cases are the maximum in this sample
ma.cent.wave.3.idx.max <- tsgc::argmax(ma.cent.new.cases) %>%
                            zoo::index()

ma.cent.wave.3.idx.max

# Create the object to be plotted - column 1 has the actual daily data and
# column 2 has the centred moving average
d <- cbind(diff(Y), ma.cent.new.cases)
names(d) <- c('New Cases', 'Centered 7-day MA')

d.df <-
  data.frame(Date = index(d),
             New.Cases = coredata(d[,1]),
             Centered.7.day.MA = coredata(d[,2]))

## ----make-fig-1, echo = FALSE, fig.cap="Figure 1: New Cases and their centered 7-day moving average for Gauteng province in South Africa between 10 March 2020 and 5 January 2022."----
# Create the plot
data_plot<-ggplot(data = d.df,
                  aes(x = Date)) +
  geom_line(aes(y = New.Cases, color = "New Cases"),
            linewidth=0.1) +
  geom_line(aes(y = Centered.7.day.MA,
                color = "Centered 7 day MA"), linewidth=1) +
  scale_y_continuous(n.breaks = 10) +
  xlab("Day") +
  ylab("New cases") +
  scale_x_date(date_breaks = "60 days") +
  scale_color_manual(name='', # Data: Gauteng
                     values=c('New Cases'='blue',
                              'Centered 7 day MA'='red'))+
  theme_light(base_size = 12)+    #18
  theme(legend.position = c(.2, 0.85))+
  theme(legend.title=element_text(size=2),
        legend.text=element_text(size=10))+ #13
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), #16
        plot.title = element_text(face = "bold"))

data_plot

## ----set_options, eval = FALSE------------------------------------------------
#  file.path = here()
#  res.dir <- here::here(file.path, 'results')
#  date.format <- "%Y-%m-%d"
#  Y = gauteng
#  estimation.date.start = as.Date("2021-02-01")
#  estimation.date.end = as.Date("2021-04-19")
#  n.forecasts = 14
#  q = 0.005
#  confidence.level = 0.68
#  plt.length = 30

## ----select_data, eval=F------------------------------------------------------
#  idx.est =
#      (zoo::index(Y) >= estimation.date.start) &
#      (zoo::index(Y) <= estimation.date.end)
#  y = Y[idx.est]

## ----model_est_free_q, eval = F-----------------------------------------------
#  model_q <- SSModelDynamicGompertz$new(Y = y)
#  res_q <- model_q$estimate()

## ----model_est_fix_q, eval = F------------------------------------------------
#  model = SSModelDynamicGompertz$new(Y = y, q = q)
#  res = model$estimate()

## ----eval_sample, eval=F------------------------------------------------------
#  y.eval = Y %>%
#  subset(index(.) > tail(res$index,1)) %>%
#  tsgc::df2ldl()

## ----plot_forecast1, eval=F---------------------------------------------------
#  tsgc::plot_forecast(
#   res=res,
#   y.eval = y.eval, n.ahead = n.forecasts,
#   plt.start.date =  tail(res$index, 1) - plt.length
#  )

## ----fig-2, echo=F, fig.cap="Figure 2: Fourteen-day forecast of ln(g_t) from 20  April 2021, for Gauteng province in South Africa."----
# Set options
date.format <- "%Y-%m-%d"
estimation.date.start <- as.Date("2021-02-01")
estimation.date.end <- as.Date("2021-04-19")
n.forecasts <- 14
q <- 0.005
confidence.level <- 0.68
plt.length <- 30

# Estimate model
idx.est <-
  (zoo::index(Y) >= estimation.date.start) &
  (zoo::index(Y) <= estimation.date.end)
y <- Y[idx.est]

# Cumulative Y over the estimation window is small y
# May not be ideal as in the paper we use small y for diff(Y)

model <- SSModelDynamicGompertz$new(Y = y, q = q)
res <- model$estimate()

# Specify evaluation dataset
y.eval <- Y %>%
  subset(zoo::index(.) > tail(res$index,1)) %>%
  tsgc::df2ldl()

# y.eval is LDL

# Plot filtered, forecast and realised log growth rate
p1 <- tsgc::plot_forecast(
  res=res,
  y.eval = y.eval, n.ahead = n.forecasts,
  plt.start.date =  tail(res$index, 1) - plt.length
)
p1

## ----fig-3, fig.cap = "Figure 3: Fourteen-day forecast of new cases from 20 April 2021 for Gauteng province in South Africa"----
tsgc::plot_new_cases(
  res, Y=y,
  n.ahead=n.forecasts,
  confidence.level=confidence.level,
  date_format="%Y-%m-%d",
  plt.start.date = tail(res$index, 1) - plt.length
)

## ----fig-4, fig.cap = "Figure 4: Accuracy of the fourteen-day forecast of new cases from 20 April 2021 for Gauteng province in South Africa."----
tsgc::plot_holdout(res, Y=y,
                   Y.eval = Y[(tail(res$index,1)+0:n.forecasts)],
                   confidence.level = 0.68,
                   date_format = "%Y-%m-%d")

## ----calc_rt, eval = F--------------------------------------------------------
#  r.t <- tail(exp(res$get_gy_ci()*gen_int),7)

## ----fig-5, echo = F, fig.cap = "Figure 5: Reproduction numbers for the 7-day period to 19 April 2021, for Gauteng province in South Africa."----
# Set generation interval
gen_int <- 4

# Compute R_t and upper/lower bounds of credible intervals following sec 2.5
r.t <- tail(exp(res$get_gy_ci()*gen_int),7) %>% tk_tbl
r.t$name <- "Gauteng"
names(r.t)<-c("Date", "Rt", "lower", "upper", "name")
r.t

# Create the plot
res.rt <- ggplot(r.t, aes(x = Date)) +
  ylim(0, 1.4)+
  geom_line(aes(y = Rt, color = "Rt")) +
  geom_point(aes(y = Rt), color = "red", size = 3) +
  geom_segment(aes(xend = Date, yend = lower, y = Rt), color = "blue") +
  geom_segment(aes(xend = Date, yend = upper, y = Rt), color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper,
                  fill = "68%  Interval"), alpha = 0.2) +
  geom_hline(yintercept = 1, linetype="solid",
             linewidth=1.5, color = "black") +
  scale_x_date(date_breaks = "1 day") +
  theme_light(base_size = 12)+    #18
  theme(legend.position = c(.85, 0.2))+
  theme(legend.title=element_text(size=2),
        legend.text=element_text(size=10))+ #13
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), #16
        plot.title = element_text(face = "bold"))

res.rt

## ----write_results, eval=F----------------------------------------------------
#  tsgc::write_results(
#   res=res,
#   res.dir = res.dir,
#   Y=Y,
#   n.ahead = n.forecasts,
#   confidence.level =  confidence.level
#  )

## ----change-est-date-end, eval=F----------------------------------------------
#  estimation.date.end = as.Date("2021-06-25")

## ----fig-6, echo = F, fig.cap = "Figure 6: Trigger for reinitialization: when filtered slope exceeds twice its standard error above zero, reinitialization is triggered to the date when the filtered slope crossed zero."----
# Update estimation date end
estimation.date.end = as.Date("2021-06-25")

# Re-estimate model
idx.est <-
  (zoo::index(Y) >= estimation.date.start) &
  (zoo::index(Y) <= estimation.date.end)
y <- Y[idx.est]

model <- SSModelDynamicGompertz$new(Y = y, q = q)
res <- model$estimate()

# Extract the smoothed slope and its standard deviation
smoothed.slope.full <- xts::xts(res$output$alphahat[, "slope"],
                                order.by = res$index)
smoothed.P.slope <- xts::xts(res$output$P[2, 2,-1], order.by = res$index)

# smoothed slope
# plot(smoothed.slope.full)

# SD(smoothed slope)
# plot(smoothed.P.slope)

d2 <- cbind(smoothed.slope.full, sqrt(smoothed.P.slope),
            1.5 * sqrt(smoothed.P.slope),
            2 * sqrt(smoothed.P.slope))

d2.df <- data.frame(date=index(d2), coredata(d2))

colnames(d2.df) <- c("Date", "smoothed.slope",
                     "sd.smoothed.slope",
                     "sd.smoothed.slope.1.5",
                     "sd.smoothed.slope.2")

d2.df <- d2.df[d2.df$Date >= as.Date("2021-02-15"), ]
# Is the date restriction needed?

trigger.df <- d2.df %>%
  mutate(prev_smoothed.slope = lag(smoothed.slope)) %>%
  filter((smoothed.slope > sd.smoothed.slope.2 &
            prev_smoothed.slope < sd.smoothed.slope.2))
# Triggered on May 01 when smoothed slope > 2* sd(smoothed slope)

reinit_zero.df <- d2.df %>%
  mutate(prev_smoothed.slope = lag(smoothed.slope)) %>%
  filter(Date < min(trigger.df$Date) &
           (smoothed.slope > 0 & prev_smoothed.slope < 0)) %>%
  arrange(desc(Date)) %>%
  slice(1)
# Reinitialisation on April 21

# Create plot
ggplot(data = d2.df,
       aes(x = Date)) +
  geom_line(aes(y = smoothed.slope,
                color = "smoothed.slope"), linewidth=.5) +
  geom_line(aes(y = sd.smoothed.slope,
                color = "sd.smoothed.slope"),
            linetype = "solid",
            linewidth=.25) +
  geom_line(aes(y = sd.smoothed.slope.1.5,
                color = "sd.smoothed.slope.1.5"),
            linetype = "solid",
            linewidth=.25) +
  geom_line(aes(y = sd.smoothed.slope.2,
                color = "sd.smoothed.slope.2"),
            linetype = "solid",
            linewidth=.5) +
  scale_y_continuous(n.breaks = 10) +
  geom_hline(yintercept = 0,
             linetype = "solid",
             color = "black",linewidth = 1)+
  geom_vline(data = trigger.df,
             aes(xintercept = Date),
             linetype = "solid", size = .5,
             color = "black")+
  geom_vline(data = reinit_zero.df,
             aes(xintercept = Date),
             linetype = "solid",size = 1,
             color = "black")+
  xlab("Day") +
  ylab("Slope") +
  scale_x_date(date_breaks = "10 days") +
  scale_color_manual(name='',
                     values=c('smoothed.slope'='red',
                              'sd.smoothed.slope'='blue',
                              'sd.smoothed.slope.1.5'='green',
                              'sd.smoothed.slope.2'='black'))+
  theme_light(base_size = 11)+
  #theme(legend.position = c(.88, 0.25))+
  theme(legend.title=element_text(size=2),
        legend.text=element_text(size=6))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(face = "bold"))

## ----set_reinit_date, eval=F--------------------------------------------------
#  reinit.dates = "2021-04-29"

## ----reinit_estim, eval=F-----------------------------------------------------
#  model <- SSModelDynGompertzReinit$new(
#    Y = y, q = q,
#    reinit.date = as.Date(reinit.dates,
#    format = date.format)
#  )
#  res.reinit <- model$estimate()

## ----reinit-y-eval, eval=F----------------------------------------------------
#  y.eval.reinit <- Y %>%
#    reinitialise_dataframe(., reinit.dates) %>%
#    df2ldl() %>%
#    subset(index(.) > tail(res.reinit$index,1))

## ----plot_forecast2, eval=F---------------------------------------------------
#  tsgc::plot_forecast(
#      res=res.reinit,
#      y.eval = y.eval.reinit,
#      n.ahead = n.forecasts,
#      plt.start.date =  tail(res.reinit$index, 1) - plt.length,
#      title='Forecast of $\ln(g_t)$  after reinitialization.'
#  )

## ----fig-7, echo=F, fig.cap = "Figure 7: Forecast of ln(g_t) after reinitialization."----
# Set reinitialization date
reinit.dates = "2021-04-21"

# Estimate reinitialized model
model <- SSModelDynGompertzReinit$new(
  Y = y, q = q,
  reinit.date = as.Date(reinit.dates, format = date.format)
)
res.reinit <- model$estimate()

# Reinitialize dataframe
Y.reinit <- Y %>%
  reinitialise_dataframe(., reinit.dates)

# Reinitialize the evaluation data (log growth rate)
y.eval.reinit <- Y %>%
  reinitialise_dataframe(., reinit.dates) %>%
  df2ldl() %>%
  subset(zoo::index(.) > tail(res.reinit$index,1))

# Create plot
tsgc::plot_forecast(
  res=res.reinit,
  y.eval = y.eval.reinit,
  n.ahead = n.forecasts,
  plt.start.date =  tail(res.reinit$index, 1) - plt.length
)

## ----fig-8, fig.cap="Figure 8: Forecast of new cases after reinitialization."----
tsgc::plot_new_cases(
  res.reinit, Y=Y.reinit, n.ahead=n.forecasts,
  confidence.level=confidence.level,
  date_format="%Y-%m-%d",
  plt.start.date = tail(res.reinit$index, 1) - plt.length
)

## ----fig-9, fig.cap="Figure 9: Forecast accuracy of the reinitialized model over the hold-out sample period: 14 days past 25 June 2021."----
tsgc::plot_holdout(res = res.reinit, Y=Y.reinit[index(y)],
                   Y.eval = Y[(tail(res.reinit$index,1)+0:n.forecasts)],
                   confidence.level = 0.68,
                   date_format = "%Y-%m-%d")

## ----fig-10, echo=FALSE, fig.cap = "Figure 10: Forecast accuracy of the model without reinitialization over the hold-out sample period: 14 days from 25 June 2021."----
tsgc::plot_holdout(res = res, Y=y,
                   Y.eval = Y[(tail(res$index,1)+0:n.forecasts)],
                   confidence.level = 0.68,
                   date_format = "%Y-%m-%d")

