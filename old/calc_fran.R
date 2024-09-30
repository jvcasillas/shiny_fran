library(lubridate)

calc_fran <- function(biological_sex = "male", bw, cj, pu) {

  # Model coefficients
  a    <-  6.7610
  b_cj <- -0.0054
  b_pu <- -0.0106
  b_bw <-  0.0025

  # Linear predictor
  fran_secs <- round(exp(a + (b_cj * cj) + (b_pu * pu) + (b_bw * bw)))

  # Convert to minutes
  fran_mins <- ms(paste("0:", as.character(fran_secs)), roll = T)

  # Store results to list
  fran_output <- list(
    biological_sex = biological_sex, 
    seconds = fran_secs, 
    minutes = fran_mins
    )

  # Output results
  return(fran_output)

}

hold <- calc_fran(bw = 180, cj = 200, pu = 20) 

