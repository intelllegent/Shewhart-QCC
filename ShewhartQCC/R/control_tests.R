

#'@title Test №1
#'@description 1 point is outside
#'
#'@export
#'
Shewchart_1 <- function(table, sample, ucl, lcl) {
  data_UCL <- NULL
  data_LCL <- NULL

  if (lcl > ucl) {
    buf <-  lcl
    lcl <-  ucl
    ucl <-  buf
  }

  s_UCL <- s_Sequence_up(sample, ucl)
  s_LCL <- s_Sequence_low(sample, lcl)

  id_UCL <- v_Position("1", s_UCL)
  if (length(id_UCL) != 1) {
    data_UCL <- table[id_UCL, ]
  } else if (id_UCL != -1) {
    data_UCL <- table[id_UCL, ]
  }


  id_LCL <- v_Position("1", s_LCL)
  if (length(id_LCL) != 1) {
    data_LCL <- table[id_LCL, ]
  } else if (id_LCL != -1) {
    data_LCL <- table[id_LCL, ]
  }

  Fin <- do.call(rbind, list(data_LCL, data_UCL))
  return(Fin)
}


#'@title Test №2
#'@description 9 points on one side of the CL or in zone C.
#'
#'@export

Shewchart_2 <- function(table, sample, average, sigma) {
  data_zone_C <- NULL
  data_up <- NULL
  data_low <- NULL
  lower_c <- average - sigma
  upper_c <- average + sigma

  s_zone_C <- s_Sequence_in(sample, lower_c, upper_c)
  id_zone_C <- v_Position("111111111", s_zone_C)

  if (length(id_zone_C) != 1) {
    data_zone_C <- table[id_zone_C:(id_zone_C + 8), ]
  } else if (id_zone_C != -1) {
    data_zone_C <- table[id_zone_C:(id_zone_C + 8), ]
  }

  s_upper_average <- s_Sequence_up(sample, average)
  id_up <- v_Position("111111111", s_upper_average)

  if (length(id_up) != 1) {
    data_up <- table[id_up:(id_up + 8), ]
  } else if (id_up != -1) {
    data_up <- table[id_up:(id_up + 8), ]
  }

  id_low <- v_Position("000000000", s_upper_average)
  if (length(id_low) != 1) {
    data_low <- table[id_low:(id_low + 8), ]
  } else if (id_low != -1) {
    data_low <- table[id_low:(id_low + 8), ]

  }

  Fin <- do.call(rbind, list(data_zone_C, data_up, data_low))
  return(Fin)
}


#' @title Test №3
#' @description 6 consecutive points are steadily increasing or decreasing.
#'
#' @export
Shewchart_3 <- function(table, sample) {
  data_incr <- NULL
  data_decr <- NULL
  incr_decr <- NULL
  s_diff <- s_Difference(sample)

  id_diff_incr <- v_Position("11111", s_diff)
  if (length(id_diff_incr) != 1) {
    data_incr <- table[(id_diff_incr):(id_diff_incr + 5), ]
  } else if (id_diff_incr != -1) {
    data_incr <- table[(id_diff_incr):(id_diff_incr + 5), ]
  }


  id_diff_decr <- v_Position("00000", s_diff)
  if (length(id_diff_decr) != 1) {
    data_decr <- table[(id_diff_decr - 1):(id_diff_decr + 5), ]
  } else if (id_diff_decr != -1) {
    data_decr <- table[(id_diff_decr - 1):(id_diff_decr + 5), ]
  }

  incr_decr <- do.call(rbind, list(data_incr, data_decr))

  Fin <- incr_decr
  return(Fin)
}


#' @title Test №4
#' @description 14 consecutive points are alternating up and down.
#'
#' @export
Shewchart_4 <- function(table, sample) {
  data_in_dec <- NULL
  s_diff <- s_Difference(sample)

  id_in_dec <- v_Position("1010101010101", s_diff)
  if (length(id_in_dec) != 1) {
    data_in_dec <- table[(id_in_dec - 1):(id_in_dec + 14), ]
  } else if (id_in_dec !=  -1) {
    data_in_dec <- table[(id_in_dec - 1):(id_in_dec + 14), ]
  }
  Fin <- data_in_dec
  return(Fin)
}


#' @title Test №5
#' @description 12 out of 3 consecutive points are more than 2 sigmas
#' from the center line in the same direction.
#'
#' @export
Shewchart_5 <- function(table, sample, average, sigma) {
  data_zone_A_up <- NULL
  data_zone_A_low <- NULL
  data_zone_A_up_2 <- NULL
  data_zone_A_low_2 <- NULL

  s_zone_A_up <- s_Sequence_up(sample, average + 2 * sigma)
  s_zone_A_low <- s_Sequence_low(sample, average - 2 * sigma)

  id_zone_A_up <- v_Position("11", s_zone_A_up)
  if (length(id_zone_A_up) != 1) {
    data_zone_A_up <- table[id_zone_A_up:(id_zone_A_up + 2), ]
  } else if (id_zone_A_up != -1) {
    data_zone_A_up <- table[id_zone_A_up:(id_zone_A_up + 2), ]
  }

  id_zone_A_low <- v_Position("11", s_zone_A_low)
  if (length(id_zone_A_low) != 1) {
    data_zone_A_low <- table[id_zone_A_low:(id_zone_A_low + 2), ]
  } else if (id_zone_A_low != -1) {
    data_zone_A_low <- table[id_zone_A_low:(id_zone_A_low + 2), ]
  }

  id_zone_A_up_2 <- v_Position("101", s_zone_A_up)
  if (length(id_zone_A_up_2) != 1) {
    data_zone_A_up <- table[id_zone_A_up_2:(id_zone_A_up_2 + 2), ]
  } else if (id_zone_A_up_2 != -1) {
    data_zone_A_up_2 <- table[id_zone_A_up_2:(id_zone_A_up_2 + 2), ]
  }

  id_zone_A_low_2 <- v_Position("101", s_zone_A_low)
  if (length(id_zone_A_low_2) != 1) {
    data_zone_A_up_2 <- table[id_zone_A_up_2:(id_zone_A_up_2 + 2), ]
  } else if (id_zone_A_low_2 != -1) {
    data_zone_A_low_2 <- table[id_zone_A_low:(id_zone_A_low + 2), ]
  }

  Fin <-
    do.call(
      rbind,
      list(
        data_zone_A_up,
        data_zone_A_low ,
        data_zone_A_up_2,
        data_zone_A_low_2
      )
    )
  return(Fin)
}


#' @title Test №6
#' @description 4 out of 5 consecutive points are more than 1 sigma
#' from the center line in the same direction.
#' @export
Shewchart_6 <- function(table, sample, average, sigma) {
  data_zone_B_up <- NULL
  data_zone_B_up_2 <- NULL
  data_zone_B_up_3 <- NULL
  data_zone_B_up_4 <- NULL
  data_zone_B_low <- NULL
  data_zone_B_low_2 <- NULL
  data_zone_B_low_3 <- NULL
  data_zone_B_low_4 <-  NULL

  s_zone_B_up <- s_Sequence_up(sample, average + sigma)
  s_zone_B_low <- s_Sequence_low(sample, average - sigma)

  id_zone_B_up <- v_Position("1111", s_zone_B_up)
  if (length(id_zone_B_up) != 1) {
    data_zone_B_up <- table[id_zone_B_up:(id_zone_B_up + 2), ]
  } else if (id_zone_B_up != -1) {
    data_zone_B_up <- table[id_zone_B_up:(id_zone_B_up + 2), ]
  }

  id_zone_B_up_2 <- v_Position("11101", s_zone_B_up)
  if (length(id_zone_B_up_2) != 1) {
    data_zone_B_up_2 <- table[id_zone_B_up_2:(id_zone_B_up_2 + 2), ]
  } else if (id_zone_B_up_2 != -1) {
    data_zone_B_up_2 <- table[id_zone_B_up_2:(id_zone_B_up_2 + 2), ]
  }

  id_zone_B_up_3 <- v_Position("11011", s_zone_B_up)
  if (length(id_zone_B_up_3) != 1) {
    data_zone_B_up_3 <- table[id_zone_B_up_3:(id_zone_B_up_3 + 2), ]
  } else if (id_zone_B_up_3 != -1) {
    data_zone_B_up_3 <- table[id_zone_B_up_3:(id_zone_B_up_3 + 2), ]
  }

  id_zone_B_up_4 <- v_Position("10111", s_zone_B_up)
  if (length(id_zone_B_up_4) != 1) {
    data_zone_B_up_4 <- table[id_zone_B_up_4:(id_zone_B_up_4 + 2), ]
  } else if (id_zone_B_up_4 != -1) {
    data_zone_B_up_4 <- table[id_zone_B_up_4:(id_zone_B_up_4 + 2), ]
  }

  id_zone_B_low <- v_Position("1111", s_zone_B_low)
  if (length(id_zone_B_low) != 1) {
    data_zone_B_low <- table[id_zone_B_low:(id_zone_B_low + 2), ]
  } else if (id_zone_B_low != -1) {
    data_zone_B_low <- table[id_zone_B_low:(id_zone_B_low + 2), ]
  }

  id_zone_B_low_2 <- v_Position("11101", s_zone_B_low)
  if (length(id_zone_B_low_2) != 1) {
    data_zone_B_low_2 <- table[id_zone_B_low_2:(id_zone_B_low_2 + 2), ]
  } else if (id_zone_B_low_2 != -1) {
    data_zone_B_low_2 <- table[id_zone_B_low_2:(id_zone_B_low_2 + 2), ]
  }

  id_zone_B_low_3 <- v_Position("11011", s_zone_B_low)
  if (length(id_zone_B_low_3) != 1) {
    data_zone_B_low_3 <- table[id_zone_B_low_3:(id_zone_B_low_3 + 2), ]
  } else if (id_zone_B_low_3 != -1) {
    data_zone_B_low_3 <- table[id_zone_B_low_3:(id_zone_B_low_3 + 2), ]
  }

  id_zone_B_low_4 <- v_Position("10111", s_zone_B_low)
  if (length(id_zone_B_low_4) != 1) {
    data_zone_B_low_4 <- table[id_zone_B_low_4:(id_zone_B_low_4 + 2), ]
  } else if (id_zone_B_low_4 != -1) {
    data_zone_B_low_4 <- table[id_zone_B_low_4:(id_zone_B_low_4 + 2), ]
  }

  Fin <-
    do.call(
      rbind,
      list(
        data_zone_B_up,
        data_zone_B_up_2,
        data_zone_B_up_3,
        data_zone_B_up_4,
        data_zone_B_low,
        data_zone_B_low_2,
        data_zone_B_low_3,
        data_zone_B_low_4
      )
    )
  return(Fin)
}


#' @title Test №7
#' @description 15 consecutive points are within 1 sigma of the center line.
#'
#' @export
Shewchart_7 <- function(table, sample, average, sigma) {
  data_zone_C <- NULL
  lower_c <- average - sigma
  upper_c <- average + sigma

  s_zone_C <- s_Sequence_in(sample, lower_c, upper_c)
  id_zone_C <- v_Position("111111111111111", s_zone_C)

  if (length(id_zone_C) != 1) {
    data_zone_C <- table[id_zone_C:(id_zone_C + 14), ]
  } else if (id_zone_C != -1) {
    data_zone_C <- table[id_zone_C:(id_zone_C + 14), ]
  }
  Fin <- data_zone_C
  return(Fin)
}


#' @title Test №8
#' @description 8 consecutive points on either side of the center line with not within 1 sigma.
#'
#' @export
Shewchart_8 <- function(table, sample, average, sigma) {
  data_zone_C <- NULL
  s_zone_C <-
    s_Sequence_in(sample, average - sigma, average + sigma)
  id_zone_C <- v_Position("00000000", s_zone_C)

  if (length(id_zone_C) != 1) {
    data_zone_C <- table[id_zone_C:(id_zone_C + 7), ]
  } else  if (id_zone_C != -1) {
    data_zone_C <- table[id_zone_C:(id_zone_C + 7), ]
  }
  Fin <- data_zone_C
  return(Fin)
}
