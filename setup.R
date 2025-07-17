## I store my datafiles on the cloud, 
#the below code allows me to retrieve paths speicifc to the machine I am using.
# Machine-specific external paths (I use two machines currently)
host <- Sys.info()[["nodename"]]

if (host == "ANUJ-PC") {
  base_path <- "C:/Users/Anuj Shinde/OneDrive - Australian National University"
} else if (host == "RSB-088702") {
  base_path <- "C:/Users/u8007407/OneDrive - Australian National University"
} else {
  stop("Unknown machine — please define paths for this system.")
}

# Set all paths (external or internal)
wd <- list()
wd$data <- file.path(base_path, "Git Project DataFiles",
                     "toadology","dataframes")
wd$output <- file.path(base_path, "Git Project DataFiles",
                       "toadology","output")