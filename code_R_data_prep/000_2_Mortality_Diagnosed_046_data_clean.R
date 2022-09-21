

get_ci_diana_spain <- function()
{
  diana_smr_cis    <- data.frame(read_excel('data_mortality/SMR t1dm in spain.xlsx', 'Sheet1',skip = 0))

  diana_smr_cis$smr_raw <- NA
  diana_smr_cis$smr_ci_lower <- NA
  diana_smr_cis$smr_ci_upper <- NA

  for( i in 1:nrow(diana_smr_cis))
  { # i <- 1
    term_split <- unlist(str_split(diana_smr_cis$smr[i]," \\("))
    diana_smr_cis$smr_raw[i]      <- as.numeric(term_split[1])

    term_split_2 <- unlist(str_split(term_split[2],", "))

    diana_smr_cis$smr_ci_lower[i] <- as.numeric(term_split_2[1])
    diana_smr_cis$smr_ci_upper[i] <- (term_split_2[2])

  }


  diana_smr_cis$smr_ci_upper <- as.numeric(gsub("\\)","", diana_smr_cis$smr_ci_upper))

  diana_smr_cis_final <- setDT(diana_smr_cis)[,list(smr=mean(smr_raw)
                                                    ,smr_ci_lower = quantile(smr_raw,probs=c(.025))
                                                    ,smr_ci_upper = quantile(smr_raw,probs=c(.975))
                                                    # ),by=c("year","sex")]
  ),by=c("age_start","age_end")]

  # write.csv(diana_smr_cis_final, "temp/diana_smr_cis_final.csv")

  return(diana_smr_cis_final)
}



