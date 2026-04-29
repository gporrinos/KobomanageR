

#if(zenodo_site == "sandbox") zenodo_records_link <- "https://sandbox.zenodo.org/api/records"




### -------------------- WAIT UNTIL DEPOSITION IS READY -------------------- ###


### This function stops the code until the api has stopped processing the request
WAIT_FOR_DEPOSITION <- function(id, token, timeout = 180, instance) {
  
  Sys.sleep(7)    # Create a delay to account for asynchronous 
  start <- Sys.time()
  
  repeat {
    deposition <- GET_DEPOSITION(id, token, instance)
    
    # Zenodo assigns bucket link only when deposition is ready
    if(!is.null(deposition$links$bucket)){
      return(deposition)  # ready
    }
    
    if(as.numeric(Sys.time() - start, units = "secs") > timeout){
      stop(paste0("Timeout waiting for Zenodo deposition ", id))
    }
    
    Sys.sleep(1)  # wait 1 second before polling again
  }
}







### ------------------------- WAIT FOR PUBLICATION ------------------------- ###




WAIT_FOR_PUBLICATION <- function(id, token, instance, timeout = 180){
  
  Sys.sleep(7)
  start <- Sys.time()
  
  repeat {
    deposition <- GET_DEPOSITION(id, token, instance)
    
    if(!is.null(deposition$submitted) && deposition$submitted){
      return(deposition)  # published
    }
    
    if(as.numeric(Sys.time() - start, units = "secs") > timeout){
      stop(paste0("Timeout waiting for Zenodo deposition ", id, " to finish publishing"))
    }
    
    Sys.sleep(2)  # check every 2 seconds
  }
}





