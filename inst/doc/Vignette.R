## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(randomForestVIP)
library(MASS)
library(EZtune)

## ---- warning=FALSE, message=FALSE, fig.width=4, fig.height=4, fig.align='center'----
set.seed(1234)

pcs <- partial_cor(medv ~ ., data = Boston, model = lm)
pcs$plot_y_part_cors

rv <- robust_vifs(medv ~ ., data = Boston, model = lm)
rv$plot_lin_vifs

## ---- warning=FALSE, message=FALSE, fig.width=4, fig.height=4, fig.align='center'----
set.seed(1)
m <- mtry_compare(medv ~ .,
  data = Boston, sqrt = TRUE,
  mvec = c(1, 4, 9, 13), num_var = 7
)
m$gg_model_errors
m$model_errors

## ---- warning=FALSE, message=FALSE, fig.width=6, fig.height=5, fig.align='center'----
m$gg_var_imp_error

## ---- warning=FALSE, message=FALSE, fig.width=4, fig.height=4, fig.align='center'----
g <- ggvip(m$rf9)$both_vips

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(randomForestVIP)

## ---- warning=FALSE, message=FALSE, fig.width=4, fig.height=4, fig.align='center'----
set.seed(1234)

lichen <- EZtune::lichen[, -c(1, 3:8)]

pairs(lichen[, c(16, 20, 26)])
cor(lichen[, c(16, 20, 26)])

pcs <- partial_cor(factor(LobaOreg) ~ .,
  data = lichen, model = lm,
  num_var = 15
)
pcs$plot_y_part_cors

rv <- robust_vifs(factor(LobaOreg) ~ .,
  data = lichen, model = lm,
  num_var = 15
)
rv$plot_nonlin_vifs

## ---- warning=FALSE, message=FALSE, fig.width=4, fig.height=4, fig.align='center'----
set.seed(100)
m <- mtry_compare(factor(LobaOreg) ~ .,
  data = lichen, sqrt = TRUE,
  mvec = c(1, 5, 19, 33), num_var = 7
)
m$gg_model_errors
m$model_errors

## ---- warning=FALSE, message=FALSE, fig.width=6, fig.height=5, fig.align='center'----
m$gg_var_imp_error

## ---- warning=FALSE, message=FALSE, fig.width=6, fig.height=5, fig.align='center'----
g <- ggvip(m$rf33, num_var = 12)$both_vips

