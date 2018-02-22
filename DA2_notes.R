
# draw  scatterplot with 4 dimensions
library(plotly)
p3 <- plot_ly(data = dat, z = ~Stress, x = ~Comm, y = ~Hours, opacity = 0.6)%>%add_markers(color = ~Test)
p3

Sys.setenv("plotly_username"="michaelespero")
Sys.setenv("plotly_api_key"="qGCgFHPDkVnV4kzcbGtb")


chart_link = api_create(p3, filename = "public-graph")
chart_link

install.packages('devtools') # if necessary
devtools::install_github("richarddmorey/tweetRcode", 
                         subdir = "tweetRcode")
install.packages("rtweet")

# load rtweet
library(rtweet)

## whatever name you assigned to your created app
appname <- "rtweet_token_michael"

## api key (example below is not a real key)
key <- "gQqOtEkKmgJEqRqp2SDsrJr9k"

## api secret (example below is not a real key)
secret <- "wy3PI2I5oRMwsJYQ4QTahhQ6QrnMD3dqgfQnn3ooXuY4klWKnN"

## create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)







library(broom)
set.seed(114)

glimpse(dat)

# Let's make dat contain columns 1 to 5 only since 6, 7, and 8 contain only NAs.
dat <- dat[, 1:5]

library(skimr)
skim(dat)

# Now we can get a feel for our data
library(GGally)
ggpairs(dat)

# With a correlation table we can find out which bivariate correlations are strong and which are weak.
dat_mat <- jmv::corrMatrix(data = dat, vars = c("Pnum", "Comm", "Stress", "Hours", "Test"))
dat_mat # It appears that the highest correlation with Test scores is Stress, *r* = -.68, *p* < .001.

# How about some regression analyses?
fit1 <- lm(Test ~ ., dat) # Saturated Model
sjstats::check_assumptions(fit1, as.logical = T)
plot(fit1)
summary(fit1)
augment(fit1)
tidy(fit1)
fit1_indices <- glance(fit1)



# Let's use the leaps package for regression analysis
library(leaps)

regsubsets.out <-
  regsubsets(
    Test ~ .,
    data = dat,
    nbest = 1, # 1 best model for each number of predictors
    nvmax = NULL, # NULL for no limit on number of variables
    force.in = NULL, force.out = NULL,
    method = "exhaustive"
  )
regsubsets.out

summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
regsubsets.out
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")

which.max(summary.out$adjr2)
summary.out$which[3, ]

fit2 <- lm(Test ~ Stress + Comm + Hours, data = dat)
sjstats::check_assumptions(fit2, as.logical = T)
summary(fit2)
fit2_indices <- glance(fit2)

fit3 <- lm(Test ~ Stress + Comm + Hours + Comm * Hours + Stress * Hours, dat)
sjstats::check_assumptions(fit3, as.logical = T)
summary(fit3)
fit3_indices <- glance(fit3)

fit4 <- lm(Test ~ Stress + Comm, dat)
fit4_indices <- glance(fit4)

# Bind each glance together
model_indices <- rbind(fit1_indices, fit2_indices, fit3_indices, fit4_indices)

# We'll make an index to tell our models apart and arrange them in descending order by adjusted R^2^.
model_indices <- model_indices %>%
  mutate(fit = 1:4) %>%
  arrange(desc(adj.r.squared))

model_indices

# We create a new data frame containing 1 new observation
new_student <- data.frame(Comm = 8, Stress = 7)

# Now we can feed the new data into the model and get a prediction for the individual's test score
predict(object = fit4, newdata = new_student, interval = "confidence")


# Here, we're comparing models using a *X*^2^ test.
model_comparison <- anova(fit1, fit2, fit3, fit4, test = "Chisq")

# With cross-validation
library(modelr)

df_split <-
  dat %>%
  crossv_mc(n = 50) %>%
  print()

# given a dataframe, return a model
fn_model <- function(dat) {
  lm(Test ~ ., data = dat)
}

df_split_model <- df_split %>%
  mutate(model = map(train, fn_model)) %>%
  print()


library(cowplot)
# Let's make a few plots
library(tidyverse)
library(ggthemes)

p1 <- ggplot(dat, aes(x = scale(Comm), y = scale(Test), col = scale(Stress))) +
  geom_point() + theme_tufte() + geom_smooth(method = "lm") + ggtitle("Semester 1")

p2 <- ggplot(dat, aes(x = scale(Stress), y = scale(Test), col = scale(Comm))) +
  geom_point() + theme_tufte() + geom_smooth(method = "lm") + ggtitle("Semester 1")
p1
p2
plot_grid(p1, p2)
multiplot(p1, p2, cols = 2)

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots <- length(plots)

  # Make the panel
  plotCols <- cols # Number of columns of plots
  plotRows <- ceiling(numPlots / plotCols) # Number of rows needed, calculated from # of cols

  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)

  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow <- ceiling(i / plotCols)
    curCol <- (i - 1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol))
  }
}




# Lets check fitted vs actual values
plot(
  fit$model[, 1], fit$fitted.values,
  main = "Actual vs Fitted values",
  pch = 16, col = "indianred", cex = 1.5,
  xlab = "Actual Values", ylab = "Fitted Values"
)
grid()
rstd <- round(sd(fit$residuals), 4)
legend(
  "bottomright",
  legend = paste("Residual SE = ", rstd), bty = "n"
)

# Let's try some more modeling
library(MASS)
library(broom)

stepfit <- MASS::stepAIC(
  object = fit, scope = list(upper = ~., lower = ~1),
  direction = "both", trace = F, steps = 100000
)
summary(stepfit)
glance(stepfit)
stepfit
# Bootstrapping
library(bootStepAIC)

bootfit <- bootStepAIC::boot.stepAIC(object = fit, data = dat, verbose = T, B = 10000, )
bootfit$Covariates
bootfit
?boot.stepAIC
# How about we give the caret package a try?
library(caret)
library(mlbench)
# Why aren't we cutting the data into a training and test set? Let's do it already.

inTrain <- createDataPartition(
  y = dat$Test,
  ## the outcome data are needed
  p = .5,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

str(inTrain)

training <- dat[ inTrain, ]
dim(training)
testing <- dat[-inTrain, ]
nrow(training)
nrow(testing)

ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

plsFit <- train(
  Test ~ .,
  data = training,
  method = "gbm",
  tuneLength = 15,
  trControl = ctrl,
  ## Center and scale the predictors for the training
  ## set and all future samples.
  preProc = c("center", "scale"),
  metric = "ROC"
)


jmv::linReg(
  dat, dep = "Test", blocks = list(c("Stress", "Comm")),
  fitMeasures = "r2Adj", modelTest = "f", modelComp = "f"
)
webshot::install_phantomjs()

install.packages("PhantomJS")

summary(lm(Test ~ Pnum * Hours + Pnum * Comm, dat))