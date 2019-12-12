

## FUNCTION THAT CONVERTS A GT3X FILE TO A CSV FILE
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


gt3x_2_csv <- function( gt3x_file )
  
{
  print( "Started processing file" )
  
  file_id <- substrRight( gt3x_file, 7, 5)
  
  tictoc::tic( paste( "File named", file_id, " processed" ) )
  
  header_csv( gt3x_file )
  
  save_accel( gt3x_file )
  
  dest <- substrRight( gt3x_file, j = 13 )
  
  file_id <- substrRight( gt3x_file, 7, 5 )
  
  unzipath <- paste0( dest, "/unzip")
  
  
  # Unzipped folder address
  
  
  unzip_folder_addres <- paste0( unzipath,"/", file_id )
  
  unlink( unzip_folder_addres, recursive = TRUE )
  
  tictoc::toc()
}  

