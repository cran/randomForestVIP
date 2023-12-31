% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partial_cor.R
\name{partial_cor}
\alias{partial_cor}
\title{Partial Correlations}
\usage{
partial_cor(formula, data = NULL, model = lm, num_var, ...)
}
\arguments{
\item{formula}{an object of class "\link{formula}" (or one that can be
coerced to that class): a symbolic description of the model to be fitted.}

\item{data}{a data frame containing the variables in the model.
By default the variables are taken from the environment which the model
is called from.}

\item{model}{Model to use for extraction partial correlations. Possible
model choices are lm, rpart, randomForest, and svm. Default is lm.}

\item{num_var}{Optional integer argument for reducing the number of
variables to the top 'num_var'. Should be an integer between 1 and the
total number of predictor variables in the model or it should be a
positive proportion of variables desired.}

\item{...}{Additional arguments to be passed to model as needed.}
}
\value{
A list of data.frames and useful plots for user evaluations of
  partial correlations.
}
\description{
A list of data.frames and useful plots for user evaluations of
  correlations and partial correlations of predictors with a given response.
}
\examples{
pcs <- partial_cor(Petal.Length ~ ., data = iris[-5], model = lm)
pcs$plot_y_part_cors
}
