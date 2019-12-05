
#' gt3x_2_csv_par
#' 
#' Processing multiple gt3x files at the same time
#' 
#' Processes all the files of a given folder in pararllel using foreach function
#' @param folder The folder where the files are located
#' @return a folder named csv with inside the folder where the .gt3x files are located
#' @import tictoc
#' @import foreach
#' @import parallel
#' @import doSNOW
#' @import tcltk

gt3x_2_csv_par <- function(folder, n.cores = detectCores()-1) {

  print("Preparing machine")
  
  tictoc:: tic ("Ready to process")
  
  cluster <- makeSOCKcluster(n.cores)
  
  registerDoSNOW(cluster)
  
  
  file_names<- list.files(folder,
                          pattern = ".gt3x",
                          full.names = TRUE)
  
  bar <- tkProgressBar(title = "Converting the gt3x files to csv. Progress:",
                       min = 0,
                       max = length(file_names),
                       width = 500)
  
  
  progresso <- function(n) {setTkProgressBar(bar, n, label = paste0(round(n/length(file_names)*100, 0), "% completed"))}
  opts <- list(progress = progresso)
  
  
  toc()
  
  tictoc::tic(paste("Processed", length(file_names), "files:"))
  
  print(paste("Started processing", length(file_names), "files"))
  
  foreach (i = 1:length(file_names),
           .export = c("gt3x_2_csv","substrRight", "divide_1e7","format_header", "header_csv", "read_info", "save_accel", "save_header", "transform_dates"),
           .packages = c("tictoc", "read.gt3x", "tidyverse", "data.table", "tcltk"),
           .inorder = TRUE,
           .options.snow = opts, .errorhandling = "pass") %dopar% {
             
             gt3x_2_csv(file_names[i])
             gc(verbose = FALSE)
           }
  
  tictoc::toc()
  
  stopCluster(cluster)
  
  close(bar)
  
  unlink(paste0(folder, "/unzip"), recursive = TRUE)
  
}