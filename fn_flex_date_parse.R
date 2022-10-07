#The following function accomplishes the following objeectives:
# 1. identify the date format of each value in a character vector storing dates
## -- We have some fields where multiple date formats are present in one character vector
# 2. convert the respective values into a consistent date format, rather than being a date recorded as a character value `..._clean`
# 3. extract year, month, and day from date `..._year`, `..._month`, `..._day`
# 4. record the format of the input variable `..._format`
# 5. Set recognized NA values to NA in `_...clean`
# 6. Flag values that are not recognizable (additional NA value? date format outside of those expected) in `..._format`
# 7. Flag values that could be more than one date format (e.g. 10-10-10) in `_...format`

if(!require(pacman)){install.packages("pacman")}

p_load(tidyverse,
       lubridate)

flex_date_parse <- function(input_dates,
                            all_cols = T,
                            print_env = F,
                            print_input_name = F,
                            select_cols = NULL){
  
  
  # 1. identify the date format of each value in a character vector storing dates
  ymd_dates <- ymd(input_dates)
  
  dmy_dates <- dmy(input_dates)
  
  ##posix date time variables are tricky, as date_time_parse() has trouble within a function.
  ###identify indices that are likely date time format by number of characters
  posix_indices <- which(nchar(input_dates) > 10)
  
  ###create empty vector of the same length as thhe input_dates to be filled in with parsed date time values.
  ### this allows thhe usage of this field with the coalesce() function later on
  posix_ymd_dates <- rep(as.Date(NA), length(input_dates))
  
  ###fill vector with parsed date time values in their original positions
  posix_ymd_dates[posix_indices] <- input_dates[posix_indices] %>%
    parse_date_time(orders = "YmdHMS")
  
  
  ##identifying those with a 00 which is used in some contexts to communicate missing month and/or day
  ##we want to make sure to catch these because they'll still have valid year information
  has_00_indices <- which(str_detect(input_dates, "-00") # adding the dash is a quick fix. This should probably be more flexible to allow other operators
                          # & str_detect(input_dates, "200", negate = T) #exclude those where 00 is in the middle of a year value e.g. 2001 ## COMMENTED OUT FOR NOW - was removing 2008-00-00 dates
                          & str_detect(input_dates, "t00:00:00z", negate = T)) #exclude date time values with missing time - this is a consistent pattern 
  
  ###CHECK: make sure there is only one remaining format left based on number of characters in each position (e.g. YYYYMMDD expected)
  has_00_check_format <- input_dates[has_00_indices] %>% 
    str_split("-") %>% 
    map(nchar) %>% #returns a list of vectors of character length delimited by "-" e.g. 2022-01-00 would reeturn "4" "2" "2"
    map(~paste0(., collapse = "|")) %>% #collapse vector into one value in order to be able to tabulate
    unlist() %>% 
    unique()
  
  stopifnot("Unexpected date format for values with expected missing month and/or day (YYYYMMDD expected). Please check format of dates meeting partial missing definition" = has_00_check_format == "4|2|2",
            "Multiple date formats for values with expected missing month and/or day. Please check format of dates meeting partial missing definition" = length(has_00_check_format) == 1)
  ###END CHECK
  
  ##Take first 7 characters of dates likely missing month/date and try to parse as year-month. If it parses, it is likely a year month "date" with missing day
  year_month_only_dates_short <- input_dates[has_00_indices] %>%
    str_sub(1, 7) %>%
    ym()
  
  ###create empty vector of the same length as thhe input_dates to be filled in with parsed values.
  ### this allows thhe usage of this field with the coalesce() function later on
  year_month_only_dates <- rep(as.Date(NA), length(input_dates))
  
  year_month_only_dates[has_00_indices] <- year_month_only_dates_short
  
  #to get year only values, find those that won't parse from the year_month_only_dates_short above but will parse as year in the first four characters
  ###did not parse as year-month
  not_ym_indices <- which(is.na(year_month_only_dates))
  
  #get intersect of those that have 00 (missing value) but did not parse as year month
  has_00_not_year_month_indices <- intersect(has_00_indices, not_ym_indices)
  
  ##take first 4 characters of dates likely missing month and date to try to parse as year
  year_only_dates_short <- input_dates[has_00_not_year_month_indices] %>%
    str_sub(1, 4) %>%
    as.numeric()
  
  year_only_dates <- rep(as.numeric(NA), length(input_dates))
  
  year_only_dates[has_00_not_year_month_indices] <- year_only_dates_short
  
  
  
  ##get the name of the input string as a character to be able to append it to the date column names
  input_dates_name <- deparse(substitute(input_dates))
  
  ###crosswalk for var renaming
  input_dates_name_df <- tibble("old" = c("date_clean", 
                                          "date_format", 
                                          "date_year", 
                                          "date_month", 
                                          "date_day"),
                                "new" = str_replace(old, "date", input_dates_name))
  
  
  ##create tibble to output
  date_df <- tibble(ymd_dates,
                    dmy_dates,
                    posix_ymd_dates,
                    year_month_only_dates,
                    year_only_dates) %>%
    mutate("n_formats_parsed" = rowSums(!is.na(.)), #this is to cheeck if a value could be parsed as more than one time of date format to flag and review
           "date_clean" = coalesce(ymd_dates, #the order of the coalesce inputs isn't important since we're flagging those that parse as more than one format
                                   dmy_dates,
                                   posix_ymd_dates),
           "date_raw" = input_dates,
           "date_format" = case_when(n_formats_parsed > 1 ~ "multiple-formats-possible",
                                     !is.na(ymd_dates) ~ "ymd",
                                     !is.na(dmy_dates) ~ "dmy",
                                     !is.na(posix_ymd_dates) ~ "posix-ymd",
                                     !is.na(year_month_only_dates) ~ "year-month-only",
                                     !is.na(year_only_dates) ~ "year-only"),
           "date_year" = case_when(!is.na(date_clean) ~ year(date_clean),
                                   date_format == "year-month-only" ~ year(year_month_only_dates),
                                   date_format == "year-only" ~ year_only_dates),
           "date_month" = case_when(!is.na(date_clean) ~ month(date_clean),
                                    date_format == "year-month-only" ~ month(year_month_only_dates)),
           "date_day" = day(date_clean)) %>%
    rename_at(vars(input_dates_name_df$old), ~input_dates_name_df$new) #rename columns with the name of the input date variable
  
  if(!is.null(select_cols)){date_df <- date_df[,select_cols]}
  
  if(!all_cols & is.null(select_cols)){
    
    date_df <- date_df %>%
      select(-c(ymd_dates, dmy_dates, posix_ymd_dates, n_formats_parsed, year_month_only_dates))
    
  }
  
  ifelse(!print_input_name, return(date_df), print(input_dates_name))
  
  if(dmy_dates %>%
     na.omit() %>%
     length() > 1){warning("Please check dmy dates to ensure that US-style mdy are not being parsed as dmy")}
  
  
}
