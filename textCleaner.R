files <- choose.files()
save_folder <- choose.dir()

for(varfile in files)
{
    base_filename <- basename(varfile)
    source_handler= file(varfile, 'r')
    new_file_name <- paste0(save_folder,"/",base_filename )
 
    if(new_file_name==varfile)
    {
      print("ERROR : same file being used for read and copy ")
      break
    }
    print(new_file_name)
    lines = readLines(source_handler)
    for(i in 1:length(lines))
    {
     
      tmp_line <- gsub("\"" , "", lines[i])
      new_file_name<-gsub("\\\\","/",new_file_name )
      write(tmp_line, file= new_file_name, append=TRUE)
     
    }
    close(source_handler)
  
}
