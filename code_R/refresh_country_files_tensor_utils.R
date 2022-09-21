
calculate_ex_gpu <- function(life_table)
{
  library(torch)
  device <- torch_device("cuda")

  life_table <- cbind(life_table, lx=c(NA) )
  life_table <- cbind(life_table, dx=c(NA) )
  life_table <- cbind(life_table, Lx=c(NA) )
  life_table <- cbind(life_table, Tx=c(NA) )
  life_table <- cbind(life_table, ex=c(NA) )
  life_table_gpu <- torch_tensor(life_table,  device = device)
  # life_table <- life_table_t1d
  # life_table
  # life_table$lx <- NA
  # life_table$dx <- NA

  life_table_gpu[,5][life_table_gpu[,3]==0] <- 100000
  life_table_gpu[,6][life_table_gpu[,3]==0] <- 100000 * life_table_gpu[,4][life_table_gpu[,3]==0]
  for(i in 1:max(life_table[,"age"]))
  {# i <- 1
    life_table_gpu[,5][life_table_gpu[,3]==i] <- life_table_gpu[,5][life_table_gpu[,3]==(i-1)] -  life_table_gpu[,6][life_table_gpu[,3]==(i-1)]
    life_table_gpu[,6][life_table_gpu[,3]==i] <- life_table_gpu[,5][life_table_gpu[,3]==i] * life_table_gpu[,4][life_table_gpu[,3]==i]
  }

  life_table_gpu[,7] <- torch_tensor(c(as_array(life_table_gpu$cpu())[,5][-1],0),device = device)
  life_table_gpu[,7][life_table_gpu[,3]==max(life_table_gpu[,3])] <- 0
  life_table_gpu[,7] <- life_table_gpu[,7] + 0.5 * life_table_gpu[,6]

  life_table_gpu[,8] <- 0
  life_table_gpu[,8][life_table_gpu[,3]==max(life_table_gpu[,3])] <- life_table_gpu[,8][life_table_gpu[,3]==max(life_table_gpu[,3])]

  for(i in (max(life_table[,3])-1):0)
  { # i <- 1
    life_table_gpu[,8][life_table_gpu[,3]==i] <- life_table_gpu[,8][life_table_gpu[,3]==(i+1) ]+ life_table_gpu[,7][life_table_gpu[,3]==i ]
  }


  life_table_gpu <- as_array(life_table_gpu$cpu())
  life_table_gpu[,9] <- ifelse(life_table_gpu[,8]==0 , 0 ,life_table_gpu[,8] / life_table_gpu[,5] )
  life_table_gpu <- as.data.frame(life_table_gpu)
  colnames(life_table_gpu) <- colnames(life_table)
  life_table_gpu <- life_table_gpu[ with(life_table_gpu, order(age,year, loc_id)), ]   # order properly for transforming to matrix

  return(life_table_gpu)
}



# calculate ex  life expectency at age x ---------------
calculate_ex <- function(life_table)
{
  # life_table <- dplyr::select(data_long, loc_id, year,age,qx =background_mortality_rate)
  # life_table
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



calculate_prevalence_tensor <- function(i, qB,  qT1D_n, qT1D_m, qT1D_percent_n, dDx) {
  # calculate_prevalence_array <- function(test) {
  loc_ids <- dimnames(i)[[1]]
  years   <- (1960 - 100):2040
  AGES    <- 0: 99
  MAX_AGE <- 100

  # Model compartments, all annual cohorts as a proportion of 1.
  S <-  array(NA, dim=list(length(loc_ids), length(years), MAX_AGE), dimnames=list(loc_ids,years, AGES))# S - susceptible (non-T1D)
  P <-  array(NA, dim=list(length(loc_ids), length(years), MAX_AGE), dimnames=list(loc_ids,years, AGES))# S - susceptible (non-T1D)
  D <-  array(NA, dim=list(length(loc_ids), length(years), MAX_AGE), dimnames=list(loc_ids,years, AGES))# S - susceptible (non-T1D)

  # incidence flows are reflected the following year, for individuals one year older
  i_shift <- array(NA, dim=list(length(loc_ids), length(years), MAX_AGE), dimnames=list(loc_ids,years, AGES))
  i_shift[,, 1] <- 0
  i_shift[,,-1] <- i[,,-MAX_AGE]
  i_all <- i / (1-dDx)              # includes deaths at onset
  i_all_shift <- i_shift / (1-dDx)  # shifted & includes deaths at onset

  # Track P cohorts and in/outflows in 3D array: {year}x{age}x{onset age}

  Pcohorts  <- array(NA, dim=list(length(loc_ids),length(years), MAX_AGE, MAX_AGE), dimnames=list(loc_ids,years, AGES, AGES))
  Icohorts  <- array(0, dim=list(length(loc_ids),length(years), MAX_AGE, MAX_AGE) , dimnames=list(loc_ids,years, AGES, AGES))
  PDcohorts <- array(0, dim=list(length(loc_ids),length(years), MAX_AGE, MAX_AGE) , dimnames=list(loc_ids,years, AGES, AGES))

  # The model proceeds yearwise, populating successive matrix rows.
  # Equation references below refer to the model summary.
  S[,1,1] <- 1
  P[,1,1] <- 0
  D[,1,1] <- 0
  Pcohorts[,1,1,1] <- 0

  dim_cohorts <- 'if'(length(loc_ids)==1, c(MAX_AGE, MAX_AGE),c(length(loc_ids),MAX_AGE, MAX_AGE))
  diagnal_1s_100_99 <-  array(diag(1,100), c(100,99,length(loc_ids)))
  diagnal_1s_100_99 <-  aperm(diagnal_1s_100_99, c(3,1,2))
  diagnal_1s_100_100 <-  array(diag(1,100), c(100,100,length(loc_ids)))
  diagnal_1s_100_100 <-  aperm(diagnal_1s_100_100, c(3,1,2))
  for (t in seq_along(years[-1])) {
    # t <- 1
    # for (t in 1:10) {
    # bump previous period's data along one cell (ie one year older)
    Sshift <- abind( array(1, dim=c(length(loc_ids),1)), S[,t, -MAX_AGE,drop=FALSE],along=3)
    Pshift <- abind( array(0, dim=c(length(loc_ids),1)), P[,t, -MAX_AGE,drop=FALSE],along=3)
    Dshift <- abind( array(0, dim=c(length(loc_ids),1)), D[,t, -MAX_AGE,drop=FALSE],along=3)

    PCshift <- array(0, dim=c(length(loc_ids),MAX_AGE, MAX_AGE))
    PCshift[,1,] <- 0
    PCshift[,-1,] <- Pcohorts[,t,-MAX_AGE,]


    #
    # # equation (3) - susceptible compartment, S
    S[,t+1,] <- Sshift[,1,] * (1 - i_all_shift[,t,]) * (1 - qB[,t,])

    # # equation (4) - prevalence compartment P
    P[,t+1,] <-    qT1D_percent_n[,t,]  *  Pshift[,1,]  * (1 - qT1D_n[,t,]) + i_shift[,t,] * Sshift[,1,] +
      (1-qT1D_percent_n[,t,]) *  Pshift[,1,]  * (1 - qT1D_m[,t,])
    #
    # # equation (4') - prevalence compartment P by cohort
    Ishift <-  array(NA, dim=c(length(loc_ids),MAX_AGE, MAX_AGE))


    Ishift_diag   <- i_shift[,t,] * Sshift[,1,]
    Icohorts_diag <- i[,t,] * S[,t,]
    # for(c in 1:length(loc_ids))
    # {  # c <- 1
    #   Ishift[c,,-MAX_AGE] <- diag(Ishift_diag[c,])[,-1] # Only want to shift age, not onset age
    #
    #   Icohorts[c,t,,] <- diag(Icohorts_diag[c,])
    #
    #   # PDcohorts[c,t,,] <- qT1D_percent_n[c,t,] * Pcohorts[c,t,,] * array(qT1D_n[c,t,], c(MAX_AGE, MAX_AGE)) +
    #   #   (1 - qT1D_percent_n[c,t,]) * Pcohorts[c,t,,] * array(qT1D_m[c,t,], c(MAX_AGE, MAX_AGE))
    #   # Pcohorts[c,t+1,,] <- qT1D_percent_n[c,t,] * PCshift[c,,] * array(1 - qT1D_n[c,t,], c(MAX_AGE, MAX_AGE)) + Ishift[c,,] +
    #   #   (1 - qT1D_percent_n[c,t,]) * PCshift[c,,] * array(1 - qT1D_m[c,t,], c(MAX_AGE, MAX_AGE))
    # }

    Ishift[,,-MAX_AGE] <- (Ishift_diag[,-1]) # Only want to shift age, not onset age
    Ishift[,,-MAX_AGE] <- Ishift[,,-MAX_AGE] *   diagnal_1s_100_99

    Icohorts[,t,,] <- Icohorts_diag[,]
    Icohorts[,t,,] <- Icohorts[,t,,] *   diagnal_1s_100_100


    Ishift[,,MAX_AGE]  <- 0  # NB: half-cycle adjustment populates any incidence for MAX_AGE

    PDcohorts[,t,,] <- array(qT1D_percent_n[,t,], dim_cohorts)  * Pcohorts[,t,,] * array(qT1D_n[,t,], dim_cohorts) +
                  (1 - array(qT1D_percent_n[,t,], dim_cohorts)) * Pcohorts[,t,,] * array(qT1D_m[,t,], dim_cohorts)
    Pcohorts[,t+1,,] <- array(qT1D_percent_n[,t,], dim_cohorts) * PCshift[,,] * array(1 - qT1D_n[,t,], dim_cohorts) + Ishift[,,] +
      (1 - array(qT1D_percent_n[,t,], dim_cohorts)) * PCshift[,,] * array(1 - qT1D_m[c,t,], dim_cohorts)



    # # equation (5) - death compartment D
    D[,t+1,] <- (Dshift[,1,]
                + i_all_shift[,t,] * dDx[,t,] * Sshift[,1,]
                # + qT1D[t,] * Pshift
                + qT1D_n[,t,] * Pshift[,1,] *       qT1D_percent_n[,t,]
                + qT1D_m[,t,] * Pshift[,1,] *  (1 - qT1D_percent_n[,t,])
                + Sshift[,1,] * qB[,t,] * (1 - i_all_shift[,t,]))

  }
  # final year cohort flows - same as in the loop but for final period

  t <- t + 1
  for(c in 1:length(loc_ids))
  {
      Icohorts[c,t,,]   <- diag(i[c,t,] * S[c,t,])
      PDcohorts[c,t,,]  <- Pcohorts[c,t,,] * array(qT1D_n[c,t,], c(MAX_AGE, MAX_AGE)) *      qT1D_percent_n[c,t,]  +
                         Pcohorts[c,t,,] * array(qT1D_m[c,t,], c(MAX_AGE, MAX_AGE)) * (1 - qT1D_percent_n[c,t,])
  }

  # # flows: based on unshifted versions of incidence
  I <- i * S                    # T1D incidence
  DDx <- i_all * dDx * S        # deaths at T1D onset
  # # DT1D <- (qT1D - qB) * P       # T1D-cause mortality
  DT1D <- (qT1D_n - qB) * P  *          qT1D_percent_n  +
          (qT1D_m - qB) * P  *     (1 - qT1D_percent_n)   # T1D-cause mortality
  DBGP <- P * qB                # background mortality for people w/ T1D
  DBGS <- S * qB * (1 - i_all)  # background mortality for susceptible pop'n
  # # DBGS <- S * qB  # background mortality for susceptible pop'n
  #
  # # Half-cycle adjustments - apply half of each flow in the reference
  # # year for that flow. We expect that on average, 1/2 of the flow
  # # occurs part-way through the year.
  S <- S - 0.5 * I - 0.5 * DDx - 0.5 * DBGS
  P <- P + 0.5 * I - 0.5 * (DT1D + DBGP)
  D <- D + 0.5 * (DDx + DBGS + DT1D + DBGP)
  Pcohorts <- Pcohorts + 0.5 * Icohorts - 0.5 * PDcohorts
  list(S=S, P=P, Pcohorts=Pcohorts, D=D, I=I, DDx=DDx, DT1D=DT1D,DBGP=DBGP, DBGS=DBGS)
}

complication_prevalence_tensor <- function(years,P_cohorts_level,smr_matrix_n, smr_matrix_m,qT1D_percent_n, hba1c_f=NULL,complication_parameters,disease_weights) {
  #   years=1960:2040 ; P_cohorts_level <-  P_cohorts_level[1,,,]; smr_matrix_n <- smr_matrix_n[1,,]; smr_matrix_m <- smr_matrix_m[1,,]; qT1D_percent_n <- qT1D_percent_n[1,,];hba1c_f=NULL
  # nyears          <- length(prev$years)

  start_year      <- min(years)
  end_year        <- max(years)

  data_start_year <- start_year-MAX_AGE+1
  all_years       <- seq(data_start_year, end_year)

  # if (is.null(hba1c_f)) {
  #   hba1c_f <- hba1c_function(prev$country)  # dimensions: year x age
  # }
  # hba1c_matrix <- hba1c_f(all_years)

  smr <- smr_matrix_n * qT1D_percent_n + smr_matrix_m * (1- qT1D_percent_n)
  hba1c_matrix <- (log(smr) +  1.5274 )/ 0.3545

  hba1c_matrix <- hba1c_matrix[-1,]


  comp_names              <- with(complication_parameters, c(weibull$abbrev, constant$complication))
  nweib                   <- nrow(complication_parameters$weibull)
  nconst                  <- nrow(complication_parameters$constant)

  # 4D comp_prev array tabulates complication prevalence by time, complication,
  # age, and age at diagnosis. Dimensions are `year` x `complication` x `age` x
  # `age at diagnosis`. Values are numbers of individuals in the reference
  # population. That is, a value of 1,000 would indicate 1,000 individuals in
  # the reference country, at that age and cohort.
  comp_prev <- array(NA,
                     dim=list(length(comp_names), length(years), MAX_AGE, MAX_AGE),
                dimnames=list(comp_names               , years , AGES   , AGES))

  # Weibull time-to-event complications
  risk <- array(0,
                dim=c(length(comp_names), length(years), MAX_AGE, MAX_AGE),
                dimnames = list(comp=comp_names, year=years, age=AGES, cohort=AGES))
  can_reuse_S <- any(('uniform_ages' %in% attributes(hba1c_f)) & c(attr(hba1c_f, 'uniform_ages'), FALSE))

  for (incidence_year in seq(end_year, data_start_year)) {
    # incidence_year <- 2020
    # print(incidence_year)

    for (incidence_age in seq(0, 99 - max(0, start_year - incidence_year))) {
      # incidence_age <- 20
      # print(incidence_age)

      max_age <- min(incidence_age + end_year - incidence_year, 99)
      # hba1c array indexes - potentially starts before start_year because
      # integration starts from age of incidence
      h_length <- max_age - incidence_age + 1
      h_age_idx <- seq(incidence_age + 1, max_age + 1)
      h_year_idx <- seq(incidence_year - data_start_year + 1, incidence_year - data_start_year + h_length)
      # survivor curve starting in incidence_year at age incidence_age
      hba1cs <- hba1c_matrix[cbind(h_year_idx, h_age_idx)]
      # risk indexes are potentially shorter, only starting at start_year
      first_risk_age <- incidence_age + max(start_year - incidence_year, 0)
      first_risk_year <- max(start_year, incidence_year)
      risk_length <- h_length - (first_risk_age - incidence_age)
      keep_risk_idx <- seq(1 + first_risk_year - incidence_year, first_risk_year - incidence_year + risk_length)
      cohort_idx <- incidence_age + 1
      risk_year_idx <- seq(max(0, incidence_year - start_year) + 1, max(0, incidence_year - start_year) + risk_length)
      risk_age_idx <- seq(first_risk_age + 1, first_risk_age + risk_length)
      # risk is the complement of survival
      if(FALSE)
      {
          hba1cs <- c(15,16,17)
          hba1cs <- c(15)

          hba1cs_rep <- rep(hba1cs,10)
          dim(hba1cs_rep) <- c(length(hba1cs_rep)/10, 10)
          hba1cs_rep <- t(hba1cs_rep)

          T <- length(hba1cs)-1 # total time length (years) incl. T=0
          hs_odd <- 0.5 * (hba1cs[-1] + hba1cs[-(T+1)]) # interpolate at half-year points
          ts <- pmin(0:T, 30)  # constant hazard after 30 years

          ts <- rep(ts,10)
          dim(ts) <- c(length(ts)/10, 10)
          ts <- t(ts)

          l_even <- ts^((1 - scale)/scale) * exp(-(intercept + slope * hba1cs_rep)/scale) / scale
          l_odd <- (ts[,-1] - 0.5)^((1 - scale)/scale) * exp(-(intercept + slope * hs_odd)/scale) / scale

          Lambda <- 1/6 * (l_even[1,] + cumsum(2 * c(0, 0, l_even[1,-c(1, T+1)]) + 4 * c(0, l_odd[1,-(T+1)])))

          exp(-Lambda) # survival function
      }

      inputs <- list()
      intercept <- complication_parameters$weibull$intercept
      slope <- complication_parameters$weibull$slope
      scale <- complication_parameters$weibull$scale
      for (comp_i in seq_len(nweib)) {
        # comp_i <- 1
        # print(comp_i)
        inputs[[comp_i]] <- list(hba1c=hba1cs,intercept=intercept[comp_i],slope=slope[comp_i],scale=scale[comp_i])
      }

      weib_survival <- function(inputs) {
        # inputs <- inputs[[1]]
        hba1c <- inputs$hba1c ; intercept<- inputs$intercept; slope<- inputs$slope; scale<- inputs$scale
        T <- length(hba1c)-1 # total time length (years) incl. T=0
        hs_odd <- 0.5 * (hba1c[-1] + hba1c[-(T+1)]) # interpolate at half-year points
        ts <- pmin(0:T, 30)  # constant hazard after 30 years
        l_even <- ts^((1 - scale)/scale) * exp(-(intercept + slope * hba1c)/scale) / scale
        l_odd <- (ts[-1] - 0.5)^((1 - scale)/scale) * exp(-(intercept + slope * hs_odd)/scale) / scale
        Lambda <- 1/6 * (l_even + cumsum(2 * c(0, 0, l_even[-c(1, T+1)]) + 4 * c(0, l_odd[-(T+1)])))
        exp(-Lambda) # survival function
      }
      S <-  lapply( inputs, weib_survival )
      # do.call(rbind, S)

      # hba1c <- inputs$hba1c ; intercept<- inputs$intercept; slope<- inputs$slope; scale<- inputs$scale
      # T <- length(hba1c)-1 # total time length (years) incl. T=0
      # hs_odd <- 0.5 * (hba1c[-1] + hba1c[-(T+1)])
     # pweibull(hba1c, shape = -slope[1], scale = scale[1])

      for (comp_i in seq_len(nweib)) {
        # comp_i <- 1
        # print(comp_i)
        risk[cbind(comp=comp_i, year=risk_year_idx, age=risk_age_idx, cohort=cohort_idx)] <- (1 - S[[comp_i]])[keep_risk_idx]
      }
    }
  }

  # This is Graham's and Gabriel's 'journeys' adjustments.
  # Reduces ON and PR by survivor shares of RF and BL, respectively,
  # effectively making this a multistate model (rather than a pure
  # time-to-event model)
  risk['ON',,,] <- risk['ON',,,] * (1 - risk['RF',,,])
  risk['PR',,,] <- risk['PR',,,] * (1 - risk['BL',,,])
  for (comp_i in seq_len(nweib)) {
    comp_prev[comp_i,,,] <- P_cohorts_level * risk[comp_i,,,]
  }

  # constant risk complications
  for (comp_i in seq_len(nconst)) {
    const_risk <- complication_parameters$constant[comp_i,]
    lt8 <- 1e-2 * const_risk$hba1c_lt_8
    gt8lt9 <- 1e-2 * const_risk$hba1c_8_to_9
    gt9 <- 1e-2 * const_risk$hba1c_gteq_9
    cr <- function(h) ifelse(h < lt8, lt8, ifelse(h < 9, gt8lt9, gt9))
    rsk <- apply(hba1c_matrix[-(1:99),], MARGIN=1:2, FUN=cr)
    for (cohort_i in seq_len(MAX_AGE)) {
      risk[nweib + comp_i,,,cohort_i] <- rsk
    }
    comp_prev[nweib + comp_i,,,] <- risk[nweib + comp_i,,,] * P_cohorts_level
  }
  # structure(list(
  #   year_comp_age_cohort=comp_prev,
  #   year_comp_age_cohort_prev_risk=risk
  # ), class='complications')
  #
  props <- risk

  disease_weights <- as.list(disease_weights)
  t1d <- disease_weights$T1D

  # Calculate disability weights to apply to prevalence. Since 100% of prevalent
  # cases have T1D, we apply the T1D disability weight directly. For all other
  # complications, we scale by the modeled complication probability.
  # Terminology note: disease weight == disability weight
  dsp <- props["DSP",,,] * disease_weights$DSP
  pr <- props["PR",,,] * disease_weights$PR + props["BL",,,] * disease_weights$BL
  on <- props["ON",,,] * disease_weights$ON
  #rf <- (props["RF_transplant",,,] * disease_weights$Transplant
  #  + props["RF_dialysis",,,] * disease_weights$Dialysis)
  rf <- props["RF",,,] * disease_weights$Dialysis
  uoa <- props["UoA",,,] * disease_weights$UoA
  hom <- props["HoM",,,] * disease_weights$HoM
  nfMI <- props["nfMI",,,] * disease_weights$nfMI
  nfCBVD <- props["nfCBVD",,,] * disease_weights$nfCBVD
  # disability weights are combined multiplicatively and applied to prevalence level
  disability_wts <- (
    1 - (1 - t1d) * (1 - dsp) * (1 - pr) * (1 - on - rf) * (1 - uoa) *
      (1 - hom) * (1 - nfMI) * (1 - nfCBVD))
  disability <- disability_wts * P_cohorts_level
  structure(
    list(
      year=apply(disability, 1, sum),
      year_age=apply(disability, c(1, 2), sum),
      year_cohort=apply(disability, c(1, 3), sum),
      year_age_cohort = disability
    ),
    class='DALYs')

}
