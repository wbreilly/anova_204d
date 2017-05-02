# PSC204d, Spring 2017
# Lab 4

################################
####Converting Data Formats#####
################################

# Wide to long

# Long data has a line for each repeated measure within person
# Wide data has a line for all repeated measures within one line

# Load in data
lead = read.table("/Users/Tim/Desktop/UCDavis/Year_3/PSC_204D/lead.txt")
names(lead) = c("ID", "Treatment", "Week0", "Week1", "Week4", "Week6")

# Convert wide to long
long.lead = reshape(lead, idvar = "ID", varying = list(3:6), v.names = "Response", timevar = "Week", direction = "long")
long.lead = long.lead[order(long.lead$ID),] # Sort data by ID variable

# Long format is necessary for fitting more complex models (e.g. mixed models)

# Long to Wide
wide.lead = reshape(long.lead, idvar = c("ID", "Treatment"), timevar = "Week", direction = "wide")