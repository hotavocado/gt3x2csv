## FUNCTION THAT CONVERTS A GT3X FILE IN CSV FILE
# Path refers to the folder where the GT3X files are stored

#' @title gt3x_2_csv
#' 
#' @description Converts a given .gt3x file to .csv format
#' 
#' @details Reads both the .txt file and the .bin file located inside the .gt3x file given by actilife software and converts it to a csv file in the save format of the .csv file extracted from the sofrtware.
#' @param gt3x_file the path to the given file 
#' @export
#' @seealso gt3x_folder_2_csv converts a folder 
#' @seealso gt3x_2_csv_par converts a a folder using paralell processing


gt3x_2_csv <- function(gt3x_file) {
  
  
  #' @title substrRight
  #' 
  #' @description Extracting n characters from the end of the string "jumping" last j characters
  #' @param x the string to be manipulated
  #' @param n the number of characters to extract from the string
  #' @param j the number of characters to ignore in the end of the string
  
  substrRight <- function( x, n = nchar( x ) - j, j = 0){
    substr( x, nchar( x ) - ( n + j ) + 1, nchar( x ) - j)
  }
  
  #' @title transform_dates
  #' 
  #' @description Changing the formatting of the date-time information
  #' 
  #' @details Util to change the format of the date-times to the format used in the .gt3x file
  #' @param x the object containing the date to be changed.
  
  transform_dates <- function(x) {
    as.POSIXct(x, origin = "0001-01-01", tz = "UTC")
  }
  
  
  #' @title divide_1e7 
  #' 
  #' @description util function to the right format of dates
  #' @param y value to be divided by 1e7
  
  divide_1e7 <- function(y) {
    y / 1e7
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
  
  read_info <- function(file_txt) {
    
    info_file <- data.frame(key = readLines(file_txt)) %>%
      separate(key, c("key", "value"), ": ") %>%
      spread(key, value)
    
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
  #' @param infofile default = info_filedf data frame containing the metadata generated through the read_info function
  #' @param dest_csv  default = ddestination folder. the folder to which you want to generate the header file
  #' @param files_list_i the name of the file that is going to be saved
  #' @importFrom hms "as_hms"
  
  
  
  save_header <- function(df_file, dest_csv, file_id)
  {
    #formatting the metadata to the actilife header form
    
    header_txt <- paste0( "------------ Data File Created By ActiGraph GT3X+ ActiLife v6.13.4 Firmware v1.9.2 date format dd/MM/yyyy at", df_file$Sample.Rate, "Hz  Filter Normal -----------\n",
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
    
    cat(header_txt, file = paste0(dest_csv, "/", file_id, "RAW.csv"))
  }
  
  #' @title header_csv
  #' 
  #' @description Reads the metadata from gt3x files and savaes as csv
  #' 
  #' @details Reads the metadata from the txt file located inside the .gt3x file provided by actigraph using the read_info function and saves it as a csv document using the save_header function.
  #' @param origin the path to the .gt3xfile to be converted
  #' @param dest default = same directory of the data.  the destination were the .csv file is going to be placed (to be implemented)
  
  
  header_csv <- function(file) {
    
    # Unzipping the file 
    read.gt3x::unzip_single_gt3x(file, location = paste0(destPath, '/unzip'), verbose = FALSE)
    
    #Txt address
    file_txt <- paste0(destPath, '/unzip/', file_id, "/info.txt")
    
    info_filedf <- read_info(file_txt)
    
    save_header(df_file = info_filedf, dest_csv = destPath, file_id = file_id)
    
    message("Header saved as: ", file_id, "RAW.csv")
    
  }
  
  ## Saves acceleration data in the same format as Actilife RAW csv output
  
  #' @title save_accel
  #' 
  #' @description  Saves acceleration of the given file 
  #' 
  #' @details Reads the binary data inside the .gt3x file and saves it in .csv format
  #' @param acc.file the path to te .gt3x file 
  
  save_accel <- function(acc.file) {
    
    message("Reading acceleration: ", file_id, '.gt3x')
    
    # Reading acceleration
    accel_df <- read.gt3x::read.gt3x(acc.file,
                                     imputeZeroes = FALSE)
    
    accel_df <- as.data.frame(accel_df[ ,-4])
    
    #accel_df$X <- as.character(accel_df$X)
    #accel_df$Y <- as.character(accel_df$Y)
    #accel_df$Z <- as.character(accel_df$Z)
    
    names(accel_df) <- c("Accelerometer Y", "Accelerometer X", "Accelerometer Z")
    
    accel_df <- accel_df[c(2,1,3)]
    
    #Writing acceleration data in csv:
    
    message("Writing acceleration: ", file_id, '.gt3x')
    
    tictoc::tic("Acceleration written")
    
    data.table::fwrite( x = accel_df,
                        file = paste0(destPath, "/", file_id, "RAW.csv"), 
                        append = TRUE,
                        sep = ",",
                        col.names = TRUE,
                        row.names = FALSE )
    
    tictoc::toc()
    
  }
  
  filePath <- normalizePath(gt3x_file)
  
  parentPath <- dirname(dirname(filePath))
  
  #create output folder, named 'csv' in parent path
  destPath <- paste0(parentPath, '/csv')
  
  if ( dir.exists(destPath) == FALSE ) {
    
    dir.create(destPath)
    
    message(paste0("csv output folder created as: ", destPath)) 
    
  } 
  
  file_id <- str_replace(basename(filePath), '\\..*$', '')
  
  message("Started processing file ", file_id, '.gt3x')
  
  tictoc::tic(paste( "File named", file_id, ".gt3x processed" ))
  
  header_csv(gt3x_file)
  
  save_accel(gt3x_file)
  
  #delete unzipped content
  unlink(paste0(destPath, "/unzip"), recursive = TRUE )
  
  tictoc::toc()
  
}  
