# R code to read Hydrus-1D Nod_Inf.out files into an R data frame
# set full path to Hydrus-1D project directory (folder)
# -=-=-=-=- EDIT THIS TO MATCH THE FOLDER ON YOUR COMPUTER ! -=-=-=-=-
PCdir <- paste0(MyPC,"ENVT5503/Hydrus1D ENVT5503/PRB")
# read Nod_Inf.out file into a vector of character (text) strings
Nod_Inf_out <- readLines(paste0(PCdir,"/","Nod_Inf.out"))
# View(Nod_Inf_out) # optionally check what we just read
# find indices of some rows we want to delete
head_rows <- grep("Node", Nod_Inf_out)
# use indices to delete the rows
Nod_Inf_out <- Nod_Inf_out[-c(seq(1,7),head_rows-2, head_rows-1,
                              head_rows+1, head_rows+2)]
# find indices of some more rows we want to delete
ends <- grep("end",Nod_Inf_out)[-1*NROW(grep("end",Nod_Inf_out))]
# use indices to delete the rows
Nod_Inf_out <- Nod_Inf_out[-c(ends, ends+1, ends+2)]
# find indices of rows that have the Hydrus-1D print times
time_rows <- grep("Time:", Nod_Inf_out)
# extract the rows with Hydrus-1D print times into a vector
timz <- Nod_Inf_out[time_rows]
# get rid of text around the Hydrus-1D print time values...
timz <- gsub("Time: ","",timz)
timz <- gsub(" ","",timz)
# ...and convert to numbers...
timz <- as.numeric(timz)
# ...then delete the Hydrus-1D print time rows
Nod_Inf_out <- Nod_Inf_out[-c(time_rows)]
# find the rows with column names for each block of print data...
head_rows <- grep("Node", Nod_Inf_out)
# and remove all except the first one
Nod_Inf_out <- Nod_Inf_out[-c(head_rows[-1])]
# strip out all but single spaces from data...
while(NROW(grep("  ", Nod_Inf_out)) > 0) {
  Nod_Inf_out <- gsub("  "," ", Nod_Inf_out)
}
# ...and (finally!) delete the very last row (an 'end' statement)
Nod_Inf_out <- Nod_Inf_out[-1*NROW(Nod_Inf_out)]
#
# write the edited output to a file...
writeLines(Nod_Inf_out, con="./NodInf3.out")
# ...and read the file in as space-delimited
node_data <- read.table(file = "NodInf3.out", header = TRUE, sep = " ")
# the is a blank column 1; rename it to "Time"
colnames(node_data)[1] <- "Time"
# use the print times vector to make a vector with
# the relevant time repeated for each block
timz0 <- rep(timz[1], NROW(node_data)/NROW(timz))
for (i in 2:NROW(timz)) {
  timz0 <- append(timz0, rep(timz[i], NROW(node_data)/NROW(timz)))
}
# replace the Time colum with the vector we just made...
node_data$Time <- timz0
# ...and convert Time to a factor
node_data$Time <- as.factor(node_data$Time)
# remove temporary objects...
rm(list = c("i","head_rows","Nod_Inf_out","timz","timz0","ends"))
# ...and check the cleaned-up data
str(node_data)
