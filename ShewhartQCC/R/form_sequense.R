
#' @title "1" and "0" sequense receiving
#'
#' @description  Comparing inputting numeric sequense with constant operand.
#' Receiving logic sequense of comparation.
#' Then transforming into "1" and "0" sequense
#'
#' Functions return "1" and "0" string, type char. Returns "1" if inputting
#' value greater than constant operand. Returns "0" if inputting
#' value lower than constant operand.
#'
#' @param sample - numeric vector
#' @param operand - constant numeric value
#'
#' @return numeric the estimated amount of good honey
#' @export
#' @examples
#' const <- 5
#' seq <- c(1:10)
#' s_Sequence_up(seq, const)
#' #"0000011111"


s_Sequence_up <- function(sample, operand){
  l_Operand <- sample > operand
  b_Operand <- 1 * l_Operand
  fin <- paste0(b_Operand, collapse = "")
  return(fin)
}

#' "1" and "0" sequense receiving
#'
#' Comparing inputting numeric sequense with constant operand.
#' Receiving logic sequense of comparation
#' Then transforming into "1" and "0" sequense
#'
#' Functions return "1" and "0" string, type char
#'
#' @param sample - numeric vector
#' @param operand - constant numeric value
#' @return numeric the estimated amount of good honey
#' @export
#' @examples
#' const <- 5
#' seq <- c(1:10)
#' s_Sequence_up(seq, const)
#' #"0000011111"

### Сравнение "<": если больше значение больше операнда - 0, меньше - 1
s_Sequence_low <- function(sample, operand){
  l_Operand <- sample < operand
  b_Operand <- 1 * l_Operand
  fin <- paste0(b_Operand, collapse = "")
  return(fin)
}

#' "1" and "0" sequense receiving
#'
#' Comparing inputting numeric sequense with constant operand.
#' Receiving logic sequense of comparation
#' Then transforming into "1" and "0" sequense
#'
#' Functions return "1" and "0" string, type char
#'
#' @param sample - numeric vector
#' @param operand - constant numeric value
#' @return numeric the estimated amount of good honey
#' @export
#' @examples
#' const <- 5
#' seq <- c(1:10)
#' s_Sequence_up(seq, const)
#' #"0000011111"

### Вывод значений, находящихся внутри границ: (operand1, operand2)
s_Sequence_in <- function(sample, operand1, operand2){
  if (operand1 > operand2){
    buf <- operand1
    operand1 <- operand2
    operand2 <- buf
  }
  l_Operand1 <- sample > operand1
  l_Operand2 <- sample < operand2
  l_Operand <- l_Operand1 & l_Operand2
  b_Operand <- 1 * l_Operand
  fin <- paste0(b_Operand, collapse = "")
  return(fin)
}

#' "1" and "0" sequense receiving
#'
#' Comparing inputting numeric sequense with constant operand.
#' Receiving logic sequense of comparation
#' Then transforming into "1" and "0" sequense
#'
#' Functions return "1" and "0" string, type char
#'
#' @param sample - numeric vector
#' @param operand - constant numeric value
#' @return numeric the estimated amount of good honey
#' @export
#' @examples
#' const <- 5
#' seq <- c(1:10)
#' s_Sequence_up(seq, const)
#' #"0000011111"
s_Difference <-  function(sample){
  v_diff <- diff(sample)
  l_diff <- v_diff > 0
  b_diff <- 1 * l_diff
  fin <- paste0(b_diff, collapse = "")
  return(fin)
}

#' "1" and "0" sequense receiving
#'
#' Comparing inputting numeric sequense with constant operand.
#' Receiving logic sequense of comparation
#' Then transforming into "1" and "0" sequense
#'
#' Functions return "1" and "0" string, type char
#'
#' @param sample - numeric vector
#' @param operand - constant numeric value
#' @return numeric the estimated amount of good honey
#' @export
#' @examples
#' const <- 5
#' seq <- c(1:10)
#' s_Sequence_up(seq, const)
#' #"0000011111"
v_Position <- function (exp, string){
  list_pos <- gregexpr(exp, string)
  v_pos <- unlist(list_pos)
  return(v_pos)
}
