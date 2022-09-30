#This script produces two data frames (that can be joined) showing all data transfers (year and quarter),
## excel files within those transfers, and sheets in those excel files for ISAR NJH data. 
##This serves dual purposes:
## 1. documenting thhe historic NJH raw data
## 2. provided the basis to iterate over to compare subsequent data transfers

# The overall approach is
## 1. get all data transfer folders
## 2. iterate over data transfers to get all .xlsx files within (including subdirectories)
## 3. output data frame of data transfers and files in each respective folder - usa_files
## 4. iterate over files listed in #3 to get the sheets in each .xlsx file
## 5. output data frame with data transfer, file name, and sheet name - usa_sheets


## 1. get all data transfer folders
#get folders
usa_folders <- paste(njh_data_folder, "2. COUNTRIES", "USA", "Data Transfers", sep = "/") %>% 
  list.files() %>%
  str_subset("NJH")



#get all excel files in each folder - create crosswalk tibble usa_files for each folder and file (includes subdirectories)
for(i in 1:length(usa_folders)){
  
  current_folder <- usa_folders[i]
  
  usa_folder_full_path <- paste(njh_data_folder, "2. COUNTRIES", "USA", "Data Transfers", current_folder, sep = "/") 
  
  excel_files <- list.files(usa_folder_full_path, recursive = T) %>%
    str_subset(".xlsx")
  
  
  temp_df <- tibble("data_upload" = current_folder,
         "file" = excel_files)
  
  if(!exists("usa_files")){
    
    usa_files <- temp_df
    
  } else{usa_files <- usa_files %>% bind_rows(temp_df)}
}



#get all sheets for each excel file
for(i in 1:nrow(usa_files)){
  
  data_upload <- usa_files$data_upload[i]
  current_file <- usa_files$file[i]
  
  current_file_full_path <- paste(njh_data_folder, "2. COUNTRIES", "USA", "Data Transfers", data_upload, current_file, sep = "/")
  
   tryCatch({
    current_file_sheets <- excel_sheets(current_file_full_path)
    
    temp_sheets_df <- tibble("data_upload" = data_upload,
                             "file" = current_file,
                             "sheets" = current_file_sheets)
    
    if(!exists("usa_sheets")){
      
      usa_sheets <- temp_sheets_df
      
    } else{usa_sheets <- usa_sheets %>% bind_rows(temp_sheets_df)}}, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
}




write_csv(usa_files,
          here("NJH", "Files", "NJH_data_files.csv"))

write_csv(usa_sheets,
          here("NJH", "Files", "NJH_data_files_w_sheets.csv"))
