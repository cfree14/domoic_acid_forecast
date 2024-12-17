

# GLORYS data access site
# https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/services
# https://help.marine.copernicus.eu/en/articles/7983226-copernicus-marine-toolbox-cli-get-original-files

# RCMEMS package
# https://github.com/markpayneatwork/RCMEMS

# Example use of RCMEMS package
# https://theoceancode.netlify.app/post/dl_env_data_r/

# Install RCMEMS package
# devtools::install_github("markpayneatwork/RCMEMS")

# Packages
library(RCMEMS)


# Example one - from RCMEMS package
##########################################################################################

# #Setup a configuration object, using OSTIA as an example
cfg <- RCMEMS::CMEMS.config(motu="https://nrt.cmems-du.eu/motu-web/Motu",
                            python="/Library/Frameworks/Python.framework/Versions/3.12/bin/python3",
                            script="/Library/Frameworks/Python.framework/Versions/3.12/bin/motuclient",
                            service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
                            product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST",
                            variable = c("analysed_sst","analysis_error"),
                            user = "cfree",
                            pwd = "Science1234!")

# Download data
RCMEMS::CMEMS.download(cfg,
                       ROI = c(8,13,55,59),
                       # date.range = c(ISOdate(2001,08,01),ISOdate(2001,08,10)),
                       date.min = "2017-01-10",
                       date.max = "2017-01-15",
                       out.path="test.nc",
                       debug=FALSE)


# Example two - from RCMEMS package
##########################################################################################

# Script
GLORYS_script <- 'python ~/motuclient-python/motuclient.py --motu http://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -180 --longitude-max 179.9166717529297 --latitude-min -80 --latitude-max 90 --date-min "2018-12-25 12:00:00" --date-max "2018-12-25 12:00:00" --depth-min 0.493 --depth-max 0.4942 --variable thetao --variable bottomT --variable so --variable zos --variable uo --variable vo --variable mlotst --variable siconc --variable sithick --variable usi --variable vsi --out-dir data --out-name test.nc --user cfree --pwd Science1234!'

# Take the chunk of code above and turn it into something useful
cfg <- RCMEMS::parse.CMEMS.script(GLORYS_script, parse.user = T)

# This is where one should make any required changes to the subsetting of the data
# This is now the magic of the RCMEMS package, which allows us to interface with the Python code as though it were R
cfg_update <- RCMEMS::update(cfg, variable = "thetao --variable bottomT --variable so --variable zos --variable uo --variable vo --variable mlotst --variable siconc --variable sithick --variable usi --variable vsi",
                             longitude.min = "-80.5",
                             longitude.max = "-40.5",
                             latitude.min = "31.5",
                             latitude.max = "63.5",
                             date.min = "2017-01-10",
                             date.max = "2017-01-15",
                             out.name = "test.nc")

# Download and save the file if needed
RCMEMS::CMEMS.download(cfg_update)


# Play
##########################################################################################

# #Setup a configuration object, using OSTIA as an example
cfg <- CMEMS.config(#python="/Library/Frameworks/Python.framework/Versions/3.12/bin/python3", 
                    motu="http://my.cmems-du.eu/motu-web/Motu",
                    service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
                    product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST",
                    variable = c("analysed_sst","analysis_error"),
                    user = "cfree",
                    pwd = "Science1234!")

# Download data
CMEMS.download(cfg,
               ROI = c(8,13,55,59),
               date.range = c(ISOdate(2001,08,01), ISOdate(2001,08,10)),
               out.path="test.nc",
               debug=FALSE)





# copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1D-m --username cfree --password Science1234! -v bottomT -x -126 -X -124 -y 32 -Y 34 -t "2020-01-01" -T "2020-01-08" -o "~/Desktop/" -f example.nc --force-download
# 
# bt <- raster::brick("~/Desktop/example.nc")
# 
# raster::plot(bt)
# 
# query <- 'copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1D-m --username cfree --password Science1234! -v bottomT -x -126 -X -124 -y 32 -Y 34 -t "2020-01-01" -T "2020-01-08" -o "~/Desktop/" -f example.nc --force-download
# '
# 
# system(query, intern=F)




# Installed CLI tool
# https://help.marine.copernicus.eu/en/articles/7970514-copernicus-marine-toolbox-installation

# 


# CopernicusMarine R package
# install.packages("CopernicusMarine")
# https://github.com/pepijn-devries/CopernicusMarine

# Packages
library(CopernicusMarine)

# Downalod example
cms_download_subset(
  username = "cfree",
  password = "Science1234!",
  destination   = file.path(getwd(), "data/glorys/example.nc"),
  product       = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
  layer         = "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m",
  variable      = "sea_water_velocity",
  region        = c(-1, 50, 10, 55),
  timerange     = c("2021-01-01", "2021-01-02"),
  verticalrange = c(0, -2)
)

cms_download_subset(
  username = "cfree",
  password = "Science1234!",
  destination   = file.path(getwd(), "data/glorys/example.nc"),
  product       = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
  layer         = "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m",
  variable      = "sea_water_velocity",
  region  = c(-124, -120, 32, 42),
  timerange = c("2021-01-01", "2021-01-05"),
  verticalrange = c(0, -2)
)



# Exampple
username <- "cfree"
password <- "Science1234!"
dataset <- "cmems_mod_glo_phy_my_0.083deg_P1D-m"
variable <- "bottomT"
bbox <- c(-126, -124, 32, 34)
daterange <- c("2020-01-01", "2020-01-08")
filename <- file.path("~/Desktop/", "example.nc")

# Download GLORYS data
download_glorys <- function(username, password, dataset, variable, bbox, daterange, filename, store=T){
  
  # This is example code that can be put into terminal
  # copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1D-m --username cfree --password Science1234! -v bottomT -x -126 -X -124 -y 32 -Y 34 -t "2020-01-01" -T "2020-01-08" -o "~/Desktop/" -f example.nc --force-download
 
  # This is an example query that can be run with system()
  # query_example <- '/Library/Frameworks/Python.framework/Versions/3.12/bin/copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1D-m --username cfree --password Science1234! -v bottomT -x -126 -X -124 -y 32 -Y 34 -t "2020-01-01" -T "2020-01-08" -o "~/Desktop/" -f example.nc --force-download'
  
  # Retrieve Copernicus location
  # This doesn't work but would improve base_q
  # cm_path <- system(command="which copernicusmarine")
  
  # Build query components
  base_q <- "/Library/Frameworks/Python.framework/Versions/3.12/bin/copernicusmarine subset "
  dataset_q <- paste("-i", dataset)
  login_q <- paste("--username", username, "--password", password)
  variable_q <- paste("-v", variable)
  bbox_q <- paste("-x", bbox[1], "-X", bbox[2], "-y", bbox[3],  "-Y", bbox[4])
  daterange_q <- paste("-t", daterange[1], "-T", daterange[2])
  end_q <- "--force-download --overwrite-output-data"
  
  # Download path
  if(is.null(filename)){
    filename_use <- file.path(tempfile(), "temp.nc")
    file_q <- paste("-o", dirname(filename_temp), "-f", basename(filename_temp))
  }else{
    file_q <- paste("-o", dirname(filename), "-f", basename(filename))
    filename_use <- filename
  }
  
  # Build final query
  query <- paste(base_q, dataset_q, login_q, variable_q, bbox_q, daterange_q, file_q, end_q)

  # Submit query and download data
  system(query, intern=F)
  
  # If reading into environment
  if(store==T){
    # obj <- ncdf4::nc_open(filename_use)
    obj <- raster::brick(filename_use)
  }else{
    obj <- "download complete"
  }
  
  # Return
  return(obj)
  
}

# Get data
data_nc <- download_glorys(username="cfree",
                           password="Science1234!",
                           dataset="cmems_mod_glo_phy_my_0.083deg_P1D-m",
                           variable="bottomT",
                           bbox=c(-126, -115, 32, 42),
                           daterange=c("2020-01-01", "2020-01-08"),
                           filename=file.path("~/Desktop", "example.nc"))

# Convert to fahrenheit
data_f <- measurements::conv_unit(3, "K", "F")

# Plot data
raster::plot(data_nc)

# Conve



