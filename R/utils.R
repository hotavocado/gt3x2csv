#' @title substrRight
#' 
#' @description Extracting n characters from the end of the string "jumping" last j characters
#' @param x the string to be manipulated
#' @param n the number of characters to extract from the string
#' @param j the number of characters to ignore in the end of the string
#' @return character the n last characters of the string ignoring the last "j" characters
#' @example file <- "study/1155.gt3x"
#' substrRight(x = file, n = 4, j = 5)
#' 
substrRight <- function( x, n = nchar( x ) - j, j = 0){
  substr( x, nchar( x ) - ( n + j ) + 1, nchar( x ) - j)
}

#' @title divide_1e7 
#' 
#' @description util function to divide the date-time number provided by the .gt3x file by 1e7, in order to make possible the conversion to dd/mm/yyyy
#' @param y value to be divided by 1e7
#' @return double number z = y/1e7
#' @example y <- 636610608000000000
#' divide_1e7(y)
#' 

divide_1e7 <- function(y) {
  y / 1e7
}


#' @title transform_dates
#' 
#' @description Converting the date-time information to the dd-mm-yy format
#' 
#' @details Util to change the format of the date-times to the format used in the .csv file. The date-time of the txt file come in the format: number of days from 0001-01-01.
#' @param x default = output of the [divide_1e7] function. the object containing the date to be changed.
#' @return the date-ime from the .txt file in the "dd/mm/yyyy" format
#' 
#' @example date <- 63661060800
#' transform_date(date)


transform_dates <- function(x) {
  as.POSIXct(x, origin = "0001-01-01", tz = "UTC")
}


#### READ_INFO function

#' @title Read Info 
#' 
#' @description Reads the metadata of the gt3x file 
#' @importFrom magrittr "%>%"
#' @importFrom tidyr "separate"
#' @importFrom tidyr "spread"
#' @details Reads the metadata registered in the .txt file that is contained inside .gt3x file provided by the actilife software
#' @param file_txt The path to the desired .txt file
#' @returns A dataframe with the necessary information to generate the .csv file header
#' @example file <- system.file("extdata", "info_example.txt", package = "gt3x2csv") 
#' read_info(file)

read_info <- function(file_txt = file_txt) {
  
  info_file <- data.frame(key = readLines(file_txt)) %>%
    tidyr::separate(key, c("key", "value"), ": ") %>%
    tidyr::spread(key, value)
  
  info_file %>%
    rename_all(funs(make.names((names(info_file))))) %>%
    mutate_at(vars( Download.Date, Last.Sample.Time, Start.Date, Stop.Date), as.numeric) %>%
    mutate_at(vars( Download.Date, Last.Sample.Time, Start.Date, Stop.Date), divide_1e7) %>%
    mutate_at(vars( Download.Date, Last.Sample.Time, Start.Date, Stop.Date), transform_dates)
  
}

#### SAVE HEADER FUNCTION
## Saves header in the same format as the Actilife RAW csv output.

#' @title save_header
#' 
#' @description Saves .gt3x metadata as csv header
#'
#' @details Saves the header extracted from the .gt3x file with the read_info function in the .csv extension (look at read_info function)
#' @param infofile default = info_filedf (output from the [read_info] function) data frame containing the metadata generated through the read_info function
#' @param dest_csv  default = destination folder  the folder to which you want to generate the header file
#' @importFrom hms "as_hms"
#' @return a .csv file named identificationRAW.csv wuth the header



save_header <- function(df_file = info_filedf, dest_csv = csv_folder, file_id)
{
  #formatting the metadata to the actilife header form
  
  header_txt <- paste0( "------------ Data File Created By ActiGraph GT3X+ ActiLife v6.13.4 Firmware v1.9.2 date format dd/MM/yyyy at 30 Hz  Filter Normal -----------\n",
                        "Serial Number: ", df_file$Serial.Number, "\n",
                        "Start Time ", as_hms(df_file$Start.Date), "\n",
                        "Start Date ", format(df_file$Start.Date, "%d/%m/%Y"), "\n",
                        "Epoch Period (hh:mm:ss) 00:00:00\n",
                        "Download Time ", as_hms(df_file$Download.Date), "\n",
                        "Download Date ", format(df_file$Download.Date, "%d/%m/%Y"), "\n",
                        "Current Memory Address: 0\n",
                        "Current Battery Voltage: ", sub(",", ".", df_file$Battery.Voltage),"     Mode = 12\n",
                        "--------------------------------------------------\n")
  
  # Writing the .csv document with the header
  
  cat(header_txt,
      file = paste0( dest_csv, "/", file_id, "RAW.csv"))
}

#' @title header_csv
#' 
#' @description Reads the metadata from gt3x files and savaes as csv
#' 
#' @details Reads the metadata from the txt file located inside the .gt3x file provided by actigraph using the read_info function and saves it as a csv document using the save_header function.
#' @param origin the path to the .gt3xfile to be converted


header_csv <- function( origin ) {
  
  dest <- substrRight( origin, j = 13)
  
  #file name 
  
  file_id <- substrRight( origin, 7, 5)
  
  print( file_id)
  
  # Results directory
  
  csv_folder <- paste0( dest, "/csv")
  
  if ( dir.exists( csv_folder) == FALSE ) {
    
    dir.create( csv_folder)
    
    message( "csv output folder created as: ", csv_folder) 
    
  } else {
    
    message( "csv folder already exists")
    
  }
  
  # Unzipping the file 
  
  read.gt3x:: unzip.gt3x( origin, location = paste0( dest, "/unzip"), remove_original = FALSE)
  
  
  # Creating the path to the desired .txt file
  
  # Unzipped folder address
  
  unzipath <- paste0( dest, "/unzip")
  
  
  # Unzipped folder address
  
  
  unzip_folder_addres <- paste0( unzipath, "/", file_id)
  
  # Txt address
  
  file_txt <- paste0( unzip_folder_addres, "/info.txt")
  
  info_filedf <- read_info( file_txt)
  
  save_header( df_file = info_filedf, dest_csv = csv_folder, file_id = file_id)
  
  message("Header saved as:  ", csv_folder, "/", file_id, "RAW.csv")
}

## Saves acceleration data in the same format as Actilife RAW csv output

#' @title save_accel
#' 
#' @description  Saves acceleration of the given file 
#' 
#' @details Reads the binary data inside the .gt3x file and saves it in .csv format
#' @param acc.file the path to te .gt3x file 

save_accel <- function( acc.file ) {
  
  #file name 
  
  file_id <- substrRight( acc.file, 7, 5)
  
  message ( "Reading acceleration", file_id)
  # Reading acceleration
  
  accel <- read.gt3x::read.gt3x( acc.file,
                                 imputeZeroes = TRUE)
  
  accel_df <- as.data.frame( accel[ ,-4])
  
  accel_df$X <- as.character( accel_df$X )
  accel_df$Y <- as.character( accel_df$Y )
  accel_df$Z <- as.character( accel_df$Z )
  
  names( accel_df ) <- c( "Accelerometer X",
                          "Accelerometer Y",
                          "Accelerometer Z")
  
  # Extracting the folder path
  
  dest <- substrRight( acc.file, j = 13 )
  
  # Results directory
  
  csv_folder <- paste0( dest, "/csv")
  
  # Writing acceleration data in csv:
  
  message( "Writing acceleration", file_id)
  
  tictoc::tic( "Acceleration written ")
  data.table::fwrite( x = accel_df,
                      file = paste0( csv_folder, "/", file_id, "RAW.csv"), 
                      append = TRUE,
                      sep = ",",
                      col.names = TRUE,
                      row.names = FALSE )
  
  tictoc::toc()
  
}
