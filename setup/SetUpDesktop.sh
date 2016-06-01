### INSTALL NETWORK PRINTER: 134.83.84.30:3910 ###

######### FROM UBUNTU SOFTWARE CENTRE ############
- dropbox
- rstudio
- mendeley desktop
- skype
- meld 
- GIMP

######### FROM TERMINAL ##########################

### CHROMIUM ###
sudo apt-get install chromium-browser

### OPEN SSH SERVER ###
sudo apt-get install openssh-server
### GENERATE SSH KEY PAIR ###
## Lists the files in your .ssh directory, if they exist
# ls -al ~/.ssh
## if the directory does not exist
# mkdir ~/.ssh
## Creates a new ssh key, using the provided email as a label
# ssh-keygen -t rsa -C "claudia.vitolo@gmail.com"
## start the ssh-agent in the background
# eval "$(ssh-agent -s)"
## Agent pid 3712
# ssh-add ~/.ssh/id_rsa
## Identity added: /home/claudia/.ssh/id_rsa (/home/claudia/.ssh/id_rsa)
## To do: Upload the new key to online repos (e.g. github, gitlab, bitbucket etc.)

### GIT ###
sudo apt-get install git
git config --global user.email "claudia.vitolo@gmail.com"
git config --global user.name "Claudia Vitolo"

### LATEX ###
sudo apt-get install texlive
sudo apt-get install texlive-publishers
### TEXSTUDIO ###
wget http://download.opensuse.org/repositories/home:/jsundermeyer/xUbuntu_14.04/amd64/texstudio_2.10.0_amd64.deb
sudo dpkg -i texstudio_2.10.0_amd64.deb

### QUANTUM GIS + GRASS + UBUNTU GIS REPOS ###
sudo apt-get install python-software-properties
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt-get update
sudo apt-get install qgis python-qgis qgis-plugin-grass 

### Install R from imperial CRAN mirror ###
sudo nano /etc/apt/sources.list
# add 1 line at the bottom, then save and close
deb http://cran.ma.imperial.ac.uk/bin/linux/ubuntu trusty/
# fetch and use GPG key
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
# update the system and install dependencies and latest R
sudo apt-get update
sudo apt-get install r-base-dev
sudo apt-get install libpq-dev
sudo apt-get install libxml2-dev
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libproj-dev
sudo apt-get install libgdal1-dev
sudo apt-get install libtiff4-dev
sudo apt-get install r-cran-rjava
sudo apt-get install libudunits2-dev
sudo apt-get install libjpeg62
export R_HOME=/usr/lib/R
export R_LIBS=/usr/lib/R/site-library
export R_LIBS_USER=/usr/local/lib/R/site-library
sudo chown kz /usr/lib/R/site-library
sudo chown kz /usr/local/lib/R/site-library
### RSTUDIO DESKTOP ###
wget https://download1.rstudio.org/rstudio-0.99.484-amd64.deb
sudo dpkg -i rstudio-0.99.484-amd64.deb
# Important CRAN R PACKAGES
# open R shell and install the following packages:
list.of.packages <- c("emoa","moments","som","dtw","zoo","latticeExtra","Hmisc",
"topmodel","coda","xts","sp","lhs","XML","qualV","RPostgreSQL","foreach",
"RCurl","devtools","ggplot2","knitr","roxygen2","XML2R","rjson","rgdal",
"tgp","segmented","gridExtra","XLConnect","udunits2","outliers",
"gstat","hydroTSM","Matching","QTLRel","klaR","surveillance","polynom","car","deSolve","DEoptim")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
install.packages("raster", repos="http://R-Forge.R-project.org") # Fixed bug with rotate
install.packages("ncdf", type = "source", configure.args="--with-netcdf-include=/usr/include")
# From GITHUB
library(devtools)
install.packages("dream", repos = "http://r-forge.r-project.org")
install_github("josephguillaume/hydromad")
install_github("ramnathv/rCharts")
# INSTALL my R PACKAGES (stable releases)
install.packages("fuse", repos="http://R-Forge.R-project.org")
install_github("cvitolo/r_rnrfa", subdir="rnrfa")
install_github("cvitolo/r_hddtools", subdir="hddtools")
# INSTALL my R PACKAGES (unstable releases)
install_github("cvitolo/r_pure", subdir="pure")
install_github("cvitolo/r_CurveNumber", subdir="curvenumber")
install_github("cvitolo/r_amca", subdir="amca")


