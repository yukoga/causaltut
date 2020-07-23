# Copyright 2014 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Set figure size for ggplot2 visualization.
#' 
#' @param width figure width in  
#' 
#' @noRd 
#' 
.fig <- function(width, heigth) {
  options(repr.plot.width = width, repr.plot.height = heigth)
}

#' Set original chart theme after onedork in jupyter-theme.
#' 
#' @importFrom extrafont Roboto
#' 
#' @noRd 
#' 
.my_theme <- function() {
  return(theme(
    plot.background = element_rect(fill = "#363F4B", colour=NA),
    plot.title = element_text(color="#8C96AC", family='Roboto', size=24, hjust=0),
    plot.subtitle = element_text(color="#8C96AC", family='Roboto', size=18, hjust=0),
    plot.caption = element_text(color="#8C96AC", family='Roboto', size=18, hjust=1),
    panel.background = element_rect(fill = "#363F4B", colour = NA),
    panel.grid.major = element_line(color="#8C96AC", linetype = "dashed", size=0.8),
    panel.grid.minor = element_line(color="#8C96AC", linetype = "dashed", size=0.5),
    legend.background = element_rect(fill = "#363F4B", colour = NA),
    legend.key = element_rect(fill = "#363F4B", colour = NA),
    legend.text = element_text(color="#8C96AC", family='Roboto', size=14),
    legend.title = element_text(color="#8C96AC", family='Roboto', size=18),
    axis.text = element_text(color="#8C96AC", family='Roboto', size=14),
    axis.title = element_text(color="#8C96AC", family='Roboto', size=18),
    axis.line = element_line(color="#8C96AC"),
  ))
}

#' Get date time index.
#'
#' @param start_date The first date in format of YYYY-mm-dd. Default: 2020-01-01.
#' @param n_days The number of days for this time series. Default: 100.
#' @param by An interval to specify time series frequency. Default: 1.
#' 
#' @return The n_days length time series in the format of YYYY-mm-dd.
#'
#' @noRd
#'
.get_dates <- function(start_date = "2020-01-01",
                       n_days = 100,
                       by = 1) {
  .dates <- seq.Date(as.Date(start_date), by = by, length.out = n_days)
  return(.dates)
}

#' Get an ARIMA series data.  
#'
#' @param base_line A base line value for the target time series.
#' @param params A list of parameters for ARIMA model to generate simulation data.
#' @param total_duration The number of all the data points.
#'
#' @noRd
#'
.get_arima <- function(base_line,
                       params,
                       total_duration) {
  .arima <- base_line + arima.sim(model = params, n = total_duration)
  return(.arima)
}

#' Get a covariate matrix.  
#'
#' @param shape A matrix shape which is c(n_samples, n_features).
#' @param base_lines A vector of base line values for the target time series.
#' @param params A vector of list of parameters for ARIMA model to generate simulation data.
#' 
#' @noRd
#' 
.get_covariate <- function(shape,
                           base_lines,
                           params) {
  nrow = shape[1]
  ncol = shape[2]
  mat <- matrix(0:0, nrow, ncol)
  mat <- apply(col(mat),
               2,
               function(x) .get_arima(base_lines[x], params[x], nrow))
  print("get covariate here.")
  return(mat)
}

#' Get a target time series based on a covariate.  
#'


#' Dataset class provides utility functions to deal with sample dataset.
#'
#' @param data_matrix A matrix which contains training data for causal impact. Default: NULL.
#' @param campaign_start The relative index in which the intervention starts. Default: 71.
#' @param campaign_end The relative index in which the intervention ends. If it's 0, then the intervention will last until the end of total_duration. Default: 0.
#' 
#' @importFrom zoo zoo 
#' @export
#' 
Dataset <- function(start_date = "2020-01-01") {
  this_env <- environment()
  start_date <- start_date 
  campaign_start <- NULL 
  campaign_end <- NULL 
  training_data <- NULL

  me <- list(
    this_env = this_env,
    get_env = function() {
      return(get("this_env", this_env))
    },

    #' Get the start date of your time series.
    #'
    #' @param as_date flag to specify if you want to get campaign_start as Date. Default: FALSE
    #' @return The start date as text  (as_date=FALSE) or Date (as_date=TRUE). 
    #'
    get_start_date = function(as_date = FALSE) {
      start_date <- get("start_date", this_env)
      if (as_date) {
        start_date <- as.Date(start_date)
      }
      return(start_date)
    },

    #' Set start date
    #' 
    #' @param start_date The date when your time series starts in text. Default: 2020-01-01.
    #' 
    set_start_date = function(value) {
      return(assign("start_date", value, this_env))
    },

    #' Get campaign start date.
    #'
    #' @param as_date flag to specify if you want to get campaign_start as Date. Default: FALSE
    #' @return The campaign start date in integer (as_date=FALSE) or Date (as_date=TRUE). 
    #'
    get_campaign_start = function(as_date = FALSE) {
      start_date <- get_start_date(as_date = TRUE) 
      #start_date <- get("start_date", this_env)
      campaign_start <- get("campaign_start", this_env)
      if (as_date) {
        campaign_start <- start_date + campaign_start -1
        #campaign_start <- as.Date(start_date) + campaign_start -1
      }
      return(campaign_start)
    },

    #' Set campaign start date
    #' 
    #' @param campaign_start The days in integer on which the intervention has started as index from the beginning of given time series. 
    #' 
    set_campaign_start = function(value) {
      return(assign("campaign_start", value, this_env))
    }
  )

  assign("this", me, this_env)
  class(me) <- append(class(me), "Dataset")
  return(me)
}

#' Provide dataset for CausalImpact tutorial.
#'
#' @param shape A matrix shape which is c(n_samples, n_features). Default: c(100, 1).
#' @param base_lines A vector of base line values for the target time series. Default: c(100).
#' @param params A vector of list of parameters for ARIMA model to generate simulation data. Default: list(ar=1.0). 
#' 
#' @noRd
#' 
#load_data.Dataset <- function(object,
                              #shape = c(100, 1),
                              #base_lines = c(100),
                              #params = c(list(ar = 0.999))) {
  #print('Concrete definition of load_data.')
  #object$training_data <- .get_covariate(shape = shape,
                                         #base_lines = base_lines,
                                         #params = params) 
  #return(object)
#}
#' Provide dataset for CausalImpact tutorial.
#'
#' @param shape A matrix shape which is c(n_samples, n_features). Default: c(100, 1).
#' @param base_lines A vector of base line values for the target time series. Default: c(100).
#' @param params A vector of list of parameters for ARIMA model to generate simulation data. Default: list(ar=1.0). 
#' @export
#' @noRd
#' 
#load_data <- function(object,
                      #shape = c(100, 1),
                      #base_lines = c(100),
                      #params = c(list(ar = 0.999))) {
  #print('general function before calling UseMethod.')
  #UseMethod("load_data", object)
#}

#load_data.default <- function(object,
                              #shape = c(100, 1),
                              #base_lines = c(100),
                              #params = c(list(ar = 0.999))) {
  #print('Default function is called.')
  #return(object)
#}


#' Definition of adding covariate method.
#'
#' @param object The dataset object which you try to add a covariate to.
#' @param base_line A base line value for the target time series. Default: 100.
#' @param params A list of parameters for ARIMA model to generate simulation data. Default: list(ar=1.0). 
#' @param total_duration The number of all the data points. Default: 100.
#' 
#' @noRd
#'
#add_covariate.Dataset <- function(object,
                                  #base_line = 100,
                                  #params = list(ar = 1.0),
                                  #totalduration = 100) {
  #if (object$training_data == NULL) {
    #stop("You need to create an initial dataset in advance.")
  #}
  #X <- object$training_data
  #x <- .get_arima(base_line, params, totalduration)
  #X <- zoo(cbind(X, x), index(X))
  #colnames(X)[-1] <- paste("x", dim(X)[2], sep="")
  #object$training_data <- X
#}

#' Default of adding covariate method.
#'
#' @param object The dataset object which you try to add a covariate to.
#' @param base_line A base line value for the target time series. Default: 100.
#' @param params A list of parameters for ARIMA model to generate simulation data. Default: list(ar=1.0). 
#' @param total_duration The number of all the data points. Default: 100.
#' 
#' @noRd
#'
#add_covariate.default <- function(object,
                                  #base_line = 100,
                                  #params = list(ar = 1.0),
                                  #totalduration = 100) {
  #return(object)
#}

#' The method to add covariate to the Dataset.
#'
#' @param object The dataset object which you try to add a covariate to.
#' @param base_line A base line value for the target time series. Default: 100.
#' @param params A list of parameters for ARIMA model to generate simulation data. Default: list(ar=1.0). 
#' @param total_duration The number of all the data points. Default: 100.
#' @export 
#'
#add_covariate <- function(object,
                          #base_line = 100,
                          #params = list(ar = 1.0),
                          #totalduration = 100) {
  #UseMethod("add_covariate", object)
#}

#' @param start_date The first date in format of YYYY-mm-dd. Default: 2020-01-01.
#' @param orig_mean The mean of normal distribution in the original time series. Default: 0.0.
#' @param orig_sd The standard deviation of normal distribution in the original time series. Default: 1.0.
#' @param orig_ampl The amplitude which multiplies on covariates to generate target metrics. Default: 2.0.
#' @param impact_ATE The true ATE(a.k.a. Average Treatment Effect) which you're going to inference with CausalImpact. Default: 0.2.
#' @param impact_sd The true standard deviation of normal distribution which generates random metrics adding to target metrics. Default: 0.05.  
#' @param seed The random seed to generate sample data. Default: 123.
#' 
#' @return Two time series in the form of matrix (y, x1, days) where y is target metrics which will be impacted by an intervention. x1 is a covariate which is used for modeling caunter factual. days is a date index for those two time series.  
#' 
#' @importFrom zoo zoo
#' @export


#load_data <- function(total_duration=100,
                      #start_date="2020-01-01",
                      #orig_mean=0.0,
                      #orig_sd=1.0,
                      #orig_ampl=2.0,
                      #impact_ATE=0.2,
                      #impact_sd=0.05,
                      #seed=123) {
  ## Create toy data
  #set.seed(seed)
  
  #if (!campaign_end) {
      #campaign_end=total_duration
  #}

  #x1 <- 100 + arima.sim(model=list(ar = 0.999), n=total_duration)
  #y <- orig_ampl * x1 + rnorm(
      #total_duration, mean=orig_mean, sd=orig_sd)
  #campaign_duration = campaign_end - campaign_start + 1
  #y[campaign_start:campaign_end] <- y[campaign_start:campaign_end] +
      #rnorm(campaign_duration, mean=impact_ATE * mean(y), sd=impact_sd)
  #days <- seq.Date(as.Date(start_date), by=1, length.out=total_duration)
  #return(zoo(cbind(y, x1), days))
#}

