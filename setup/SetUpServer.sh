### OS: UBUNTU SERVER 14.04 ####################################################

# The following software is pre-installed:
# Basic ubuntu server
# openssh
# PostgreSQL

######### R + RStudioServer + Shiny Server #####################################

### Install R from imperial CRAN mirror ###
sudo nano /etc/apt/sources.list
# add 1 line at the bottom, then save and close
deb https://www.stats.bris.ac.uk/R/bin/linux/ubuntu trusty/
# fetch and use GPG key
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
# update the system and install dependencies and latest R
sudo apt-get update
sudo apt-get install r-base-dev

sudo apt-get install -y libpq-dev 
sudo apt-get install -y libxml2-dev 
sudo apt-get install -y libcurl4-openssl-dev 
sudo apt-get install -y libproj-dev 
sudo apt-get install -y libgdal1-dev 
sudo apt-get install -y libtiff5-dev 
sudo apt-get install -y r-cran-rjava 
sudo apt-get install -y libudunits2-dev 
sudo apt-get install -y libjpeg62
sudo apt-get install -y libnetcdf-dev
sudo apt-get install -y netcdf-bin

export R_HOME=/usr/lib/R
export R_LIBS=/usr/lib/R/site-library
export R_LIBS_USER=/usr/local/lib/R/site-library
sudo chown claudia /usr/lib/R/site-library
sudo chown claudia /usr/local/lib/R/site-library

### RSTUDIO Server ###
sudo apt-get install gdebi-core
wget https://download2.rstudio.org/rstudio-server-0.99.489-amd64.deb
sudo gdebi rstudio-server-0.99.489-amd64.deb

### R Shiny Server (only on CAPRI - Server 1)###
sudo su - \
-c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.1.759-amd64.deb
sudo gdebi shiny-server-1.4.1.759-amd64.deb
sudo mkdir /srv/shiny-server/kehra
sudo chown claudia kehra

# Important CRAN R PACKAGES
# open R shell and install the following packages:
list.of.packages <- c("emoa","moments","som","dtw","zoo","latticeExtra","Hmisc",
"topmodel","coda","xts","sp","lhs","XML","qualV","RPostgreSQL","foreach",
"RCurl","devtools","ggplot2","knitr","roxygen2","XML2R","rjson","rgdal",
"tgp","segmented","gridExtra","XLConnect","udunits2","outliers",
"gstat","hydroTSM","Matching","QTLRel","klaR","surveillance","polynom","car",
"deSolve","DEoptim", "dplyr", "rmarkdown", "leaflet", "dplyr", "googleVis", "RNetCDF", "ncdf4")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
install.packages("raster", repos="http://R-Forge.R-project.org") # Fixed bug with rotate
#install.packages("ncdf", type = "source", configure.args="--with-netcdf-include=/usr/include") # obsolete
# From GITHUB
library(devtools)
install.packages("dream", repos = "http://r-forge.r-project.org")
install_github("josephguillaume/hydromad")
install_github("ramnathv/rCharts")
# INSTALL my R PACKAGES (stable releases)
install.packages("fuse", repos="http://R-Forge.R-project.org")
install_github("cvitolo/r_rnrfa", subdir="rnrfa")
install_github("cvitolo/r_hddtools", subdir="hddtools")

################### SET UP SERVER ACCOUNTS #####################################
# To list all users:
    cut -d: -f1 /etc/passwd

# To create a user and add it to the sudo group:
    sudo adduser <usernane> 
    sudo adduser <username> sudo

################### SET UP POSTGRESQL ##########################################
# First access
sudo -i -u postgres # later on I can login as claudia: sudo -i -u claudia
# Create a New Role for claudia, andrew, allan and generic kehrauser (all superusers)
createuser --interactive
# Create a New Database
createdb kehra
# Ctrl+D to exit postgres
# Connect as claudia to the new db, if I am sshing as claudia
psql -d kehra
# get info
\conninfo
# to change password
sudo -u postgres psql
\password claudia

# Enable server to listen to any IP
sudo nano /etc/postgresql/9.3/main/postgresql.conf
# look for line with listen_addresses, uncomment it and set it to:
#   listen.addresses='*'
sudo nano /etc/postgresql/9.3/main/pg_hba.conf
# add the following line (4th item is the ip address of my laptop)
# host    all     all     134.83.84.68    255.255.255.255 trust

# Install PostGIS extension
sudo apt-get install -y postgis postgresql-9.3-postgis-2.1
# Load the PostGIS extension on the database kehra
sudo -u postgres psql -c "CREATE EXTENSION postgis; CREATE EXTENSION postgis_topology;" kehra

# Note that to make a user a SuperUser, use the SQL query below 
# ALTER USER username WITH SUPERUSER;
# To make a user no longer a SuperUser: 
# ALTER USER username WITH NOSUPERUSER;
# To just allow the user to create a database: 
# ALTER USER username CREATEDB;

# select * from gis."GBR_adm_NUTS1GOR" union select * from gis."KAZ_adm1"
# ALTER TABLE tableName ADD id SERIAL;
# ALTER TABLE tableName ADD PRIMARY KEY (id);

### ECMWF API for python 2.7 ###################################################
sudo apt-get -y install python-pip
sudo pip2 install ecmwf-api-client
