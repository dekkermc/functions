
get_comments <- function(x){

  raw_lines <- readLines(x)
  
  comment_indices <- str_detect(raw_lines, "^#")
  
  line_numbers_4L <- sprintf("%04d", 1:length(raw_lines))
  
  line_number_length <- as.numeric(line_numbers_4L) %>% nchar() %>% max()
  
  zeroes_to_drop <- 4 - line_number_length
  
  line_numbers <- str_sub(line_numbers_4L, start = zeroes_to_drop + 1)
  
  
  
  
  lines_w_numbers <- paste0(line_numbers, ": ", raw_lines)
  
  comment_lines <- lines_w_numbers[comment_indices]
  
  return(comment_lines)

}








