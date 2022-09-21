# Utility functions
#
# Confidential
# Copyright (c) JDRF 2020, All rights reserved
#

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Convert a constant to univariate function, if needed
#'
#' This variant broadcasts the numeric to width \code{MAX_AGE}.
#'
#' @param f function or numeric constant
#' @return a function, possibly converting f to a function
#' @export
make_age_function <- function(f) {
  if(is.function(f)) {
    f
  } else {
    function(year, draw=1) {
      res <- rep(f, MAX_AGE)
      if (length(year) > 1) {
        matrix(rep(res, length(year)), ncol=100, byrow=FALSE,
               dimnames = list(year, AGES))
      } else {
        res
      }
    }
  }
}


#' Returns an array based on the function provided
#'
#' Internal function that returns an array based on the closure provided for
#' each year.
#'
#' @param f function. Can be incidence, mortality, death or population.
#' @param years numeric vector containing years to calculate statistics for.
#' @param ... additional parameters to pass through to f
#'
#' @return array
#' @export
matrix_from_function <- function(f, years, ...) {
  a <- t(sapply(years, f, simplify='matrix', ...))
  dimnames(a) <- list(years, AGES)
  a
}



#' @param data_long  with column (loc_id, year,age,value).  imputate to year :1900 - 2040, keep constant outside,  age : 0-99
#' @export
imputation_year_age <- function(data_long)
{
  # data_long <- model_smr_minimal_care
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))


  # data_long <- od_rates [od_rates$loc_id==462,]

  data_wide <-  spread(data_long, year, "value")
  data_wide_prefix    <-data_wide[,c(1,2),drop=F]
  data_wide           <-data_wide[,c(-1,-2)]



  new_cname_expand <- range(colnames(data_wide))[1] : range(colnames(data_wide))[2]
  data_wide <- data.frame(t(apply(data_wide, 1, function(x_row) spline(x=colnames(data_wide), y=log(x_row), method="natural",xout=new_cname_expand  )$y)))
  data_wide <- exp(data_wide)
  colnames(data_wide) <- new_cname_expand
  data_wide[is.nan(data_wide) ] <- 0
  # data.frame(x=colnames(data_wide), y = c(t(data_wide[1,]) )) %>% e_chart(x) %>% e_scatter(y)


  # Keep values before and after fit cuve constant --------------
  matrix <- data.frame(matrix(NA, ncol = length(1900:2040), nrow = nrow(data_wide)))
  colnames(matrix) <- as.character(1900:2040)
  matrix[,colnames(data_wide)] <- data_wide

  for(p in 1:nrow(matrix))
  {
    # p <- 1
    matrix_t <- matrix[p,]
    matrix_t_names <- names(matrix_t)[!is.na(matrix_t)]
    year_min <- min(matrix_t_names)
    year_max <- max(matrix_t_names)
    matrix_t[names(matrix_t) <= year_min] <-   matrix_t[year_min]
    matrix_t[names(matrix_t) >= year_max] <-   matrix_t[year_max]
    matrix[p,] <- matrix_t
  }

  data_wide <- matrix
  # data.frame(x=colnames(data_wide), y = c(t(data_wide[1,]) )) %>% e_chart(x) %>% e_scatter(y)

  data_wide <- cbind(data_wide_prefix,data_wide)

  data_long  <-  gather(data_wide, key = year, value ="value" , -loc_id,-age)

  data_wide <-  spread(data_long, age, value)

  data_wide_prefix    <-data_wide[,c(1,2),drop=F]
  data_wide           <-data_wide[,c(-1,-2),drop=FALSE]

  new_cname_expand <- seq(0, 99, 1)

  data_wide <- data.frame(t(apply(data_wide, 1, function(x) spline(1:ncol(data_wide), x, length(new_cname_expand))$y)))

  colnames(data_wide) <- new_cname_expand

  data_wide <- cbind(data_wide_prefix,data_wide)


  data_long  <-  gather(data_wide, key = age, value ="value" , -loc_id,-year)

  data_long$year <- as.numeric(data_long$year)
  data_long$age <- as.numeric(data_long$age)
  data_long
}




# calculate ex  life expectency at age x ---------------
calculate_ex <- function(life_table)
{
  # life_table <- dplyr::select(life_table_background, loc_id, year,age,qx =background_mortality_rate)
  # life_table <- life_table_background
  life_table$lx <- NA
  life_table$dx <- NA
  life_table$lx[life_table$age==0] <- 100000
  life_table$dx[life_table$age==0] <- 100000 * life_table$qx[life_table$age==0]
  for(i in 1:max(life_table$age))
  {# i <- 1
    life_table$lx[life_table$age==i] <- life_table$lx[life_table$age==(i-1)] -  life_table$dx[life_table$age==(i-1)]
    life_table$dx[life_table$age==i] <- life_table$lx[life_table$age==i] * life_table$qx[life_table$age==i]
  }

  life_table$Lx <- c(life_table$lx[-1],0)
  life_table$Lx[life_table$age==max(life_table$age)] <- 0
  life_table$Lx <- life_table$Lx + 0.5 * life_table$dx

  life_table$Tx <- 0
  life_table$Tx[life_table$age==max(life_table$age)] <- life_table$Lx[life_table$age==max(life_table$age)]

  for(i in (max(life_table$age)-1):0)
  { # i <- 1
    life_table$Tx[life_table$age==i] <- life_table$Tx[life_table$age==(i+1) ]+ life_table$Lx[life_table$age==i ]
  }

  life_table$ex <- ifelse(life_table$Tx==0 , 0 ,life_table$Tx / life_table$lx )

  return(life_table)
}


# calculate ex  life expectency at age x ---------------
calculate_ex_lifetime_years_lost <- function(life_table)
{  # in the case of smr is 1 :  life_table$smr <- 2
  # life_table <- t1d_mortality_long


  life_table$lx <- NA
  life_table$dx <- NA
  life_table$lx[life_table$age==0] <- 100000
  life_table$dx[life_table$age==0] <- 100000 * life_table$qx[life_table$age==0]
  for(i in 1:max(life_table$age))
  {# i <- 1
    life_table$lx[life_table$age==i] <- life_table$lx[life_table$age==(i-1)] -  life_table$dx[life_table$age==(i-1)]
    life_table$dx[life_table$age==i] <- life_table$lx[life_table$age==i] * life_table$qx[life_table$age==i]
  }

  life_table$Lx <- c(life_table$lx[-1],0)
  life_table$Lx[life_table$age==max(life_table$age)] <- 0
  life_table$Lx <- life_table$Lx + 0.5 * life_table$dx

  life_table$Tx <- 0
  life_table$Tx[life_table$age==max(life_table$age)] <- life_table$Lx[life_table$age==max(life_table$age)]

  for(i in (max(life_table$age)-1):0)
  { # i <- 1
    life_table$Tx[life_table$age==i] <- life_table$Tx[life_table$age==(i+1) ]+ life_table$Lx[life_table$age==i ]
  }

  life_table$ex <- ifelse(life_table$Tx==0 , 0 ,life_table$Tx / life_table$lx )


  #Add complications :  Life time years lost factor ---------------------------------------------------
  life_table$hba1c   <- (log(life_table$smr) +  1.5274 )/ 0.3545
  # life_table$hba1c  <- 4
  # View(life_table %>% mutate_if(is.numeric, round, digits=2))


  complication_parameters <- get_complication_parameters()
  weibull                 <- complication_parameters$weibull
  disease_weights         <- get_disease_weights()

  nweib                   <- nrow(complication_parameters$weibull)

  inputs <- list()
  intercept <- complication_parameters$weibull$intercept
  slope <- complication_parameters$weibull$slope
  scale <- complication_parameters$weibull$scale


  weib_survival <- function(hba1c, weibull,disease_weights) {
    # weibull=complication_parameters$weibull  ;  hba1c <- c(10,10,10,10,10,10)
    props <- array(0,
                  dim=c(nrow(weibull), length(hba1c)),
                  dimnames = list(comp=weibull$abbrev, age=1:length(hba1c)))

    for(i in 1:nrow(weibull))
    {
      intercept <- weibull$intercept[i]
      slope     <- weibull$slope[i]
      scale     <- weibull$scale[i]
      T <- length(hba1c)-1 # total time length (years) incl. T=0
      hs_odd <- 0.5 * (hba1c[-1] + hba1c[-(T+1)]) # interpolate at half-year points
      ts <- pmin(0:T, 30)  # constant hazard after 30 years

      # Simpson's rule
      l_even <- ts^((1 - scale)/scale) * exp(-(intercept + slope * hba1c)/scale) / scale
      l_odd <- (ts[-1] - 0.5)^((1 - scale)/scale) * exp(-(intercept + slope * hs_odd)/scale) / scale
      Lambda <- 1/6 * (l_even + cumsum(2 * c(0, 0, l_even[-c(1, T+1)]) + 4 * c(0, l_odd[-(T+1)])))

      exp(-Lambda) # survival function
      # print( exp(-Lambda) )
      props[weibull$abbrev[i],] <- 1- exp(-Lambda)


    }
    # This is Graham's and Gabriel's 'journeys' adjustments. Reduces ON and PR by survivor shares of RF and BL, respectively,
    # effectively making this a multistate model (rather than a pure time-to-event model)
    props['ON',] <- props['ON',] * (1 - props['RF',])
    props['PR',] <- props['PR',] * (1 - props['BL',])

    # Calculate disability weights to apply to prevalence. Since 100% of prevalent
    # cases have T1D, we apply the T1D disability weight directly. For all other
    # complications, we scale by the modeled complication probability.
    # Terminology note: disease weight == disability weight
    t1d <- disease_weights$T1D
    dsp <- props["DSP",] * disease_weights$DSP
    pr <- props["PR",] * disease_weights$PR + props["BL",] * disease_weights$BL
    on <- props["ON",] * disease_weights$ON
    #rf <- (props["RF_transplant",,,] * disease_weights$Transplant
    #  + props["RF_dialysis",,,] * disease_weights$Dialysis)
    rf <- props["RF",] * disease_weights$Dialysis
    uoa <- props["UoA",] * disease_weights$UoA
    hom <- props["HoM",] * disease_weights$HoM
    nfMI <- props["nfMI",] * disease_weights$nfMI
    nfCBVD <- props["nfCBVD",] * disease_weights$nfCBVD

    # disability_wts <- (
    #   1 - (1 - t1d) * (1 - dsp) * (1 - pr) * (1 - on - rf) * (1 - uoa) *
    #     (1 - hom) * (1 - nfMI) * (1 - nfCBVD))
    disability_wts <- (
      1 -  (1 - dsp) * (1 - pr) * (1 - on - rf) * (1 - uoa) *
        (1 - hom) * (1 - nfMI) * (1 - nfCBVD))

    disability_wts

  }


  setDT(life_table)[age>=10,risk:=weib_survival(hba1c=hba1c[age>=10], weibull=weibull,disease_weights),by=c("year")]
  life_table$Lx_original <- life_table$Lx


  life_table$risk[is.na(life_table$risk)] <- 0

  # T1D complication lifetime years -----------------------------------------------------------------------------------------------
  life_table$Lx <- life_table$Lx_original * life_table$risk
  life_table$Tx <- 0
  life_table$Tx[life_table$age==max(life_table$age)] <- life_table$Lx[life_table$age==max(life_table$age)]
  for(i in (max(life_table$age)-1):0)
  { # i <- 1
    life_table$Tx[life_table$age==i] <- life_table$Tx[life_table$age==(i+1) ]+ life_table$Lx[life_table$age==i ]
  }
  life_table$ex_complication <- ifelse(life_table$Tx==0 , 0 ,life_table$Tx / life_table$lx )

  # T1D treatment lifetime years ---------------------------------------------------------------------------------------------------
  life_table$Lx <-  life_table$Lx_original * disease_weights$T1D
  life_table$Tx <- 0
  life_table$Tx[life_table$age==max(life_table$age)] <- life_table$Lx[life_table$age==max(life_table$age)]
  for(i in (max(life_table$age)-1):0)
  { # i <- 1
    life_table$Tx[life_table$age==i] <- life_table$Tx[life_table$age==(i+1) ]+ life_table$Lx[life_table$age==i ]
  }
  life_table$ex_treatment <- ifelse(life_table$Tx==0 , 0 ,life_table$Tx / life_table$lx )


  return(life_table)
}






random_gen <- function(mean,smr_ci_lower, smr_ci_upper)
{
  smr <- NULL
  smr_ci_upper <- sapply(smr_ci_upper, max,0)  +1
  smr_ci_lower <- sapply(smr_ci_lower, max,0)+1
  mean         <- sapply(mean, max,0)+1
  # # normal distribution
  #  sd <-                    (smr_ci_upper - smr_ci_lower)/(1.96*2)
  #  smr1                      <- rnorm(length(mean), mean=mean, sd= sd )/1000000
  #  smr1[smr1<0] <- 0
  #  smr <- smr1
  #  # log normal distribution
  sd <-                    (log(smr_ci_upper) - log(smr_ci_lower) )/(1.96*2)
  smr3                      <- round(exp(rnorm(length(mean), mean=log(mean), sd= sd ))-1,4)
  smr <- smr3
  # # gamma
  # mean[mean==0] <-0.0001
  # smr2   <- rgamma(length(mean),shape=mean^2/sd^2,scale=sd^2/mean)/1000000
  # smr2[is.nan(smr2)] <- 0
  # # hist(smr,breaks = 200)
  # smr <- smr2
  return(smr)
}

assert <- function(condition,message)
{
  try(if(!condition) stop(message))
}
