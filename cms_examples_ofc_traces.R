# Count-Min Sketch dimensioning for Metro network telemetry. Examples.
# Jose Alberto Hernandez
# January 2023

############################################################
#####################  READING CSV
############################################################ 

library(sads)
library(jsonlite)

setwd("C:/Users/otomy/Documents/GitHub/countminsketch_P4telemetry")

name <- "input_files/packet_10000000000"
data <- read.csv(paste0("",name,".csv"))


arrival_times <- data$arrival
filtered_packets <- arrival_times[arrival_times <= 100e-3]
filtered_flow_ids <- data$flowID[arrival_times <= 100e-3]

# Calculate number of flows and packets
nflows <- length(unique(filtered_flow_ids))
flow_ids <- unique(filtered_flow_ids)  # Get the unique flow IDs
npackets <- length(filtered_packets)

# Packet characteristics
PacketSizeDistribution = c(64,596,1500) # Trimodal distribution, packet size (bytes)
PacketSizeWeights = c(4/12,2/12,6/12) # Trimodal distribution, packet weigths (percentage)

AvgPacketSize = sum(8*PacketSizeDistribution*PacketSizeWeights) # 870 Bytes, matches Poland University


############################################################
#####################  FLOW ANALYSIS 
############################################################ 

flowpacket <- numeric(nflows)  # Initialize flowpacket vector

for (ii in 1:nflows) {
  # Calculate the number of packets for each flow within the first 100ms
  flow_packets <- filtered_flow_ids == flow_ids[ii]  # Packets for flow ii
  flowpacket[ii] <- sum(flow_packets & arrival_times <= 100e-3)  # Count packets for flow ii in the first 100ms
}

# Sort flow_ids and flowpacket according to flow_ids in ascending order
sorted_indices <- order(flow_ids)  # Get the sorting indices based on flow_ids
flow_ids <- flow_ids[sorted_indices]  # Sort flow_ids
flowpacket <- flowpacket[sorted_indices]  # Sort flowpacket accordingly

# Calculate CDF for the flowpacket
total_packets <- sum(flowpacket)  # Total number of packets
CDF <- cumsum(flowpacket) / total_packets  # CDF is the cumulative sum of flowpacket normalized by total packets

# Plot the CDF for sorted flow_ids
plot(flow_ids, CDF, type = "b", pch = 19, col = "red", 
     xlab="Flow ID", ylab="Accumulated Traffic (CDF)", 
     main=paste("Flow-size CDF distribution"))

# Adding a legend
legend("bottomright", legend=c("CDF of flow sizes"), col="red", lty=1, pch=19)


############################################################
#####################  CMS sketch
############################################################

cms_calc <- function(flowpacket, data, ncols){
  
  set.seed(321)
  
  nhash = 11
  
  sshash = c()
  for(hh in c(1:nhash)) {
    sshash = rbind(sshash,sample(x=c(1:ncols), size=nflows, replace = TRUE, prob = NULL))
  }
  
  cms = 0*matrix(0,nrow=nhash,ncol=ncols)
  for (ii in c(1:nflows)) {
    for (nn in c(1:nhash)) {
      cms[nn,sshash[nn,ii]] = cms[nn,sshash[nn,ii]] + flowpacket[ii]
    }
  }
  
  
  # query de uno de ellos
  
  # error in the top-20 flows
  
  error_3 = 0*c(1:nflows)
  error_5 = 0*c(1:nflows)
  error_7 = 0*c(1:nflows)
  error_9 = 0*c(1:nflows)
  
  cms_query_3 = 0*c(1:nflows); cms_query_5 = 0*c(1:nflows); cms_query_7 = 0*c(1:nflows); cms_query_9 = 0*c(1:nflows);
  for (ii in c(1:nflows)) {
    flowid = ii #sample(x=c(1:nflows), size=1)
    
    cms_aux = 0*c(1:nhash)
    for (nn in c(1:nhash)) {
      cms_aux[nn] = cms[nn,sshash[nn,flowid]]
    }
    cms_query_3[ii] = min(cms_aux[1:3])
    cms_query_5[ii] = min(cms_aux[1:5])
    cms_query_7[ii] = min(cms_aux[1:7])
    cms_query_9[ii] = min(cms_aux[1:9])
    
    assign(paste0("cms_query_d3_W", ncols ), cms_query_3, envir = .GlobalEnv)
    assign(paste0("cms_query_d5_W", ncols ), cms_query_5, envir = .GlobalEnv)
    assign(paste0("cms_query_d7_W", ncols ), cms_query_7, envir = .GlobalEnv)
    assign(paste0("cms_query_d9_W", ncols ), cms_query_9, envir = .GlobalEnv)

    error_3[ii] = (cms_query_3[ii] - flowpacket[flowid])/flowpacket[flowid]
    error_5[ii] = (cms_query_5[ii] - flowpacket[flowid])/flowpacket[flowid]
    error_7[ii] = (cms_query_7[ii] - flowpacket[flowid])/flowpacket[flowid]
    error_9[ii] = (cms_query_9[ii] - flowpacket[flowid])/flowpacket[flowid]
    
  }

  Ntest = 20 # top-20 could be 50% of total traffic
  print(flowpacket[1:Ntest])
  print(paste("Av error",mean(error_3[1:20])*100, "%"))
  print(paste("Av error",mean(error_5[1:20])*100, "%"))
  print(paste("Av error",mean(error_7[1:20])*100, "%"))
  print(paste("Av error",mean(error_9[1:20])*100, "%"))
  
  # Define flow IDs for heavy hitters and aggregate flows
  flow_ids <- 0:(Ntest-1)
  
  # Calculate aggregate values for non-heavy hitter flows (flow ID -1)
  flow_id_minus1_packets <- sum(flowpacket[21:length(flowpacket)])
  flow_id_minus1_bitrate <- sum(flowpacket[21:length(flowpacket)] * AvgPacketSize / data$arrival[length(data$arrival)] / 1e9)
  
  # Calculate error for non-heavy hitter flows (flow ID -1)

  error_d3 <- c(error_3[1:Ntest], mean(error_3[21:length(error_3)]))
  error_d5 <- c(error_5[1:Ntest], mean(error_5[21:length(error_5)]))
  error_d7 <- c(error_7[1:Ntest], mean(error_7[21:length(error_7)]))
  error_d9 <- c(error_9[1:Ntest], mean(error_9[21:length(error_9)]))
                                         
  # Create data frame with results
  output_data <- data.frame(
    flowID = c(flow_ids, -1),
    numberofpackets_real = c(flowpacket[1:20], flow_id_minus1_packets),
    numberofpackets_3 = c(cms_query_3[1:20], sum(cms_query_3[21:length(cms_query_3)])),
    numberofpackets_5 = c(cms_query_5[1:20], sum(cms_query_5[21:length(cms_query_5)])),
    numberofpackets_7 = c(cms_query_7[1:20], sum(cms_query_7[21:length(cms_query_7)])),
    numberofpackets_9 = c(cms_query_9[1:20], sum(cms_query_9[21:length(cms_query_9)])),
    bitrate_Gbps_real = c(flowpacket[1:20] * AvgPacketSize / data$arrival[length(data$arrival)] / 1e9, flow_id_minus1_bitrate),
    bitrate_Gbps_3 = c(cms_query_3[1:20] * AvgPacketSize / data$arrival[length(data$arrival)] / 1e9, sum(cms_query_3[21:length(cms_query_3)] * AvgPacketSize / data$arrival[length(data$arrival)] / 1e9)),
    bitrate_Gbps_5 = c(cms_query_5[1:20] * AvgPacketSize / data$arrival[length(data$arrival)] / 1e9, sum(cms_query_5[21:length(cms_query_5)] * AvgPacketSize / data$arrival[length(data$arrival)] / 1e9)),
    bitrate_Gbps_7 = c(cms_query_7[1:20] * AvgPacketSize / data$arrival[length(data$arrival)] / 1e9, sum(cms_query_7[21:length(cms_query_3)] * AvgPacketSize / data$arrival[length(data$arrival)] / 1e9)),
    bitrate_Gbps_9 = c(cms_query_9[1:20] * AvgPacketSize / data$arrival[length(data$arrival)] / 1e9, sum(cms_query_9[21:length(cms_query_5)] * AvgPacketSize / data$arrival[length(data$arrival)] / 1e9)),
    error_d3 <- error_d3*100,
    error_d5 <- error_d5*100,
    error_d7 <- error_d7*100,
    error_d9 <- error_d9*100
  )
  names(output_data) <- c("Flow ID",
                          paste0("numberofpackets_real"),
                          paste0("numberofpackets_d3_W", ncols ),
                          paste0("numberofpackets_d5_W", ncols ),
                          paste0("numberofpackets_d7_W", ncols ),
                          paste0("numberofpackets_d9_W", ncols ),
                          paste0("bitrate_Gbps_real"),
                          paste0("bitrate_Gbps_d3_W", ncols ),
                          paste0("bitrate_Gbps_d5_W", ncols ),
                          paste0("bitrate_Gbps_d7_W", ncols ),
                          paste0("bitrate_Gbps_d9_W", ncols ),
                          paste0("error_d3","_W", ncols ),
                          paste0("error_d5","_W", ncols ),
                          paste0("error_d7","_W", ncols ),
                          paste0("error_d9","_W", ncols )
                          )
 # return(mean(error)*100)
  return(output_data)
}

output_data55 <- cms_calc(flowpacket,  data, ncols = 55)
output_data256 <- cms_calc(flowpacket,  data, ncols = 256)
output_data1024 <- cms_calc(flowpacket,  data, ncols = 1024)



############################################################
#####################  CVS files
############################################################

save_cvs <- function(output_data, name){

  file_name = paste0("ourput_files/traffic_results_packet_10_5050", name, ".csv")
  # Save to CSV file
  #file_name = paste0("traffic_results_", name,".csv")
  write.csv(output_data, file_name, row.names = FALSE)
  
  # Output the result file path if needed
  print("CSV file saved as 'traffic_results.csv'")
}


save_cvs(output_data55, "_W55")
save_cvs(output_data256, "_W256")
save_cvs(output_data1024, "_W1024")

