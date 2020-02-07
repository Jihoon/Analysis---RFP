compute_rfp <- function(i_data, i_mapping_region, PED=1.0, PTC=1.0, VERSION="unepfi1", CHECK=FALSE, VERBOSE=FALSE) {
  if (VERBOSE) cat("(compute_rfp) ------ [>>] Entering function... ------\n")
  
  if (VERBOSE) {
    cat("(compute_rfp) Check output of each RFP component:\n")
    print(head(rfp_direct_cost(i_data, VERSION=VERSION, CHECK=FALSE, VERBOSE=FALSE)))
    print(head(rfp_indirect_cost(i_data, VERSION=VERSION, CHECK=FALSE, VERBOSE=FALSE)))
    print(head(rfp_lcinv(i_data, VERSION=VERSION, VERBOSE=FALSE)))
  }
  
  
  a <- rfp_direct_cost(i_data, VERSION=VERSION, CHECK=CHECK, VERBOSE=VERBOSE)
  b <- rfp_indirect_cost(i_data, VERSION=VERSION, CHECK=CHECK, VERBOSE=VERBOSE)
  c <- rfp_lcinv(i_data, i_mapping_region, VERSION=VERSION, VERBOSE=VERBOSE)
  
  out <- rbind(a,b,c)
  out <- rfp_revenue(i_data, out, PED, PTC, VERSION=VERSION, VERBOSE=VERBOSE)
  
  if (VERBOSE) cat("(compute_rfp) ------ [<<] Exiting function... ------\n\n")
  
  return(out)
  
}