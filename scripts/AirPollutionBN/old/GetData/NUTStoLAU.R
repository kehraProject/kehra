# Lookup table NUTS - Local Authority code
# Source: http://ec.europa.eu/eurostat/web/nuts/local-administrative-units
# LOCAL ADMINISTRATIVE UNITS (LAU) - Correspondence table LAU 2 – NUTS 2013, EU-28 (2014)
# http://ec.europa.eu/eurostat/documents/345175/501971/EU-28_2014.xlsx
# Save the UK sheet only in csv format: /
# /home/kz/Dropbox/Projects/kehra/data/Health/EU-28_2014_UKonly.csv
# Test:
# rm(x,LAUcode,NUTScode,newCode, LAUnew, LAUold)
# NUTStoLAU(NUTScode=NULL, LAUcode = "00AC")
# NUTStoLAU(NUTScode=NULL, LAUcode = "22UK")
# NUTStoLAU(NUTScode=NULL, LAUcode = "E09000003")
# NUTStoLAU(NUTScode="UKI71", LAUcode = NULL)

NUTStoLAU <- function(NUTScode=NULL, LAUcode=NULL, verbose = FALSE){
  
  # Read in lookup table
  x <- read.csv("~/Dropbox/Projects/kehra/data/Health/EU-28_2014_UKonly.csv")
  
  if ( is.null(NUTScode) & is.null(LAUcode) ) {
    
    message("Insert at least 1 code!")
    
  }else{
    
    # Given LAUcode, lookup the corresponding NUTScode #########################
    if ( is.null(NUTScode) ){
      
      # First attempt search in column LAU1_NAT_CODE (old code)
      myCODE <- unique(as.character(x$NUTS3[which(x$LAU1_NAT_CODE == LAUcode)]))
      
      if ( length(myCODE) == 1 ){
        
        if (verbose == TRUE) message("Found a unique correspondence between old LAU and NUTS")
        
      }
      
      if ( length(myCODE) > 1 ) {
          
          message(paste("Caution 1! LAU old code:", LAUcode,
                        "corresponds to NUTS codes:", paste(myCODE, collapse = " and ")))
        
        if (verbose == TRUE) message("Maybe refer to a higher level Local Authority!")
      }
      
      if ( length(myCODE) == 0 ) {
        
        # Second attempt search in column LAU1_NAT_CODE_NEW (new code)
        myCODE <- unique(as.character(x$NUTS3[which(x$LAU1_NAT_CODE_NEW == LAUcode)]))
        
        if ( length(myCODE) == 1){
          
          if (verbose == TRUE) message("Found a unique correspondence between new LAU and NUTS")
          
        }
        
        if ( length(myCODE) > 1 ) {
          
          message(paste("Caution 2! LAU new code:", LAUcode,
                        "corresponds to NUTS codes:", paste(myCODE, collapse = " and ")))
          
          if (verbose == TRUE) message("Maybe refer to a higher level Local Authority!")
          
        }
        
        if ( length(myCODE) == 0 ) {
          
          message(paste("There is no correspondence between LAUcode", LAUcode, 
                        "and NUTS codes."))
          
          if (verbose == TRUE) message("Maybe refer to a higher level Local Authority!")
          
        }
        
      }
      
    }
    
    # Given NUTScode, lookup the corresponding LAUcode #########################
    if (is.null(LAUcode)){
      
      # Look up for old LAU code
      LAUold <- unique(as.character(x$LAU1_NAT_CODE[which(x$NUTS3 == NUTScode)]))
      
      if ( length(LAUold) == 1 ){
        
        if (verbose == TRUE) message("Found a unique correspondence between old LAU and NUTS")
        
      }
      
      if ( length(LAUold) > 1 ) {
        
        message(paste("Caution 3! NUTS code:", NUTScode,
                      "corresponds to LAU old codes:", paste(LAUold, collapse = " and ")))
        
        if (verbose == TRUE) message("Maybe refer to a higher level Local Authority!")
        
      }
      
      if ( length(LAUold) == 0 ) {
        
        message(paste("There is no correspondence between NUTScode", NUTScode, 
                      "and LAU old codes."))
        
        if (verbose == TRUE) message("Maybe refer to a higher level Local Authority!")
        
      }
      
      # Look up for new LAU code
      LAUnew <- unique(as.character(x$LAU1_NAT_CODE_NEW[which(x$NUTS3 == NUTScode)]))
      
      if ( length(LAUnew) == 1 ){
        
        if (verbose == TRUE) message("Found a unique correspondence between new LAU and NUTS")
        
      }
      
      if ( length(LAUnew) > 1 ) {
        
        message(paste("Caution 4! NUTS code:", NUTScode,
                      "corresponds to LAU new codes:", paste(LAUnew, collapse = " and ")))
        
        if (verbose == TRUE) message("Maybe refer to a higher level Local Authority!")
        
      }
      
      if ( length(LAUnew) == 0) {
        
        message(paste("There is no correspondence between NUTScode", NUTScode, 
                      "and LAU new codes."))
        
        if (verbose == TRUE) message("Maybe refer to a higher level Local Authority!")
        
      }
      
      myCODE <- list("old"= LAUold, "new"=LAUnew)
      
      }
    
    return(myCODE)
    
  }
  
}
