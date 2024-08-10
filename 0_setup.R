#--------------------------------------------------------------------------------
# setup.R         Setup project
#--------------------------------------------------------------------------------

# 1. set computer
#cpu <- "robe"
cpu <- "david"  

# Set main data paths
if(cpu == "robe") main_dir <- "C:/Users/David/SML Dropbox/gitdata/EggCase_Distribution"
if(cpu == "david") main_dir <- "C:/Users/david/SML Dropbox/gitdata/EggCase_Distribution"
setwd(main_dir)

# 2. Create data paths
input_data <- paste(main_dir, "input", sep="/")
if (!dir.exists(input_data)) dir.create(input_data, recursive = TRUE)

temp_data <- paste(main_dir, "temp", sep="/")
if (!dir.exists(temp_data)) dir.create(temp_data, recursive = TRUE)

output_data <- paste(main_dir, "output", sep="/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# 3. load CEMEMS username / password based on cpu

if(cpu == "david") f <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/user.txt"
username <- paste(readLines(f, warn = FALSE), collapse = "")
if(cpu == "david") f <- "C:/Users/david/OneDrive/Escritorio/chondrichthyan_habitat/psw.txt"
password <- paste(readLines(f, warn = FALSE), collapse = "")

if(cpu == "robe") f <- "C:/Users/David/Desktop/chondrichthyan_habitat/user.txt"
username <- paste(readLines(f, warn = FALSE), collapse = "")
if(cpu == "robe") f <- "C:/Users/David/Desktop/chondrichthyan_habitat/psw.txt"
password <- paste(readLines(f, warn = FALSE), collapse = "")


# 4. SML directory
if(cpu == "david") SML  <- "C:/Users/david/SML Dropbox"
