####################################################################
#
# Read CSV Example
#
# v1 - initial verison
#
# v2 - ...
#
####################################################################

#
# Define program defaults
#
# Program Name and Version
progname = "readcsv_v1"
#
# Debug output
debugFlag = FALSE
#
# Prompt for Files instead of using defaults
promptFileNames = FALSE
#
# Directory for sun data and illumination files
#
testDir  = "testdata"
#
# Input Sun File
#
inFile   = "in.csv"
#
# Output Files
#
outFile  = "out.csv"




root_dir<-function(){
	# Look for the root directory in order of preference
	dir1 = "C:/Wolf-Projects/Data"
	if( file.exists(dir1) ) return(dir1)
	dir1 = "D:/Wolf-Projects/Data"
	if( file.exists(dir1) ) return(dir1)
	dir1 = "E:/Wolf-Projects/Data"
	if( file.exists(dir1) ) return(dir1)
	# default
	dir1 = "C:/Data"
	return (dir1)
}

skip_line<-function(){
  message(" >>>")
}

blank_line<-function(){
  message(" ")
}

pad_int<-function(n,scale){
  out_string<-paste(10*scale + n,sep='')
  out_string<-substr(out_string,2,nchar(out_string))
  return(out_string)
}

file_name<-function(d1,d2,f){
  return(paste(d1,"/",d2,"/",f,sep=""))
}


getData<-function(){
  data = {}
  #
  # TODO - select file
  #
  fnameI  <- file_name(myDir,testDir,inFile)
  skip_line()
  if (promptFileNames) {
    message(" >>> Select: Data Input File: ")
    fnameI = file.choose()
  }
  data  = read.csv(fnameI,stringsAsFactors=FALSE)
  nrdata   = length(data[,1])
  ncdata   = length(data[1,])
  message(" >>> Processing: Data Input File: ","  Name:", fnameI,"  Rows: ",nrdata,"  Cols: ",ncdata)
  skip_line()
  return(data)
}



####################################################################
#
# Start Main Script
#
####################################################################

message(">>>>>>>>>> Create: ",progname,"  --  Time: ",Sys.time())
skip_line()
message(" >>> Checking Root Directories <<< ")
myDir = root_dir()
message(" >>> Root Dir = ",myDir)
blank_line()

###################################
# Read CSV file
# Each  column is one set of paramaters
# First column are the paramater names
data     = getData()
nrdata   = length(data[,1])
ncdata   = length(data[1,])

# paramter names
pname    = data[1]
# slice of data skipping first columns
pdata    = data[2:ncdata]

###################################
# apply has 3+ parameters
#   a dataframe or matrix
#   a value indicating which dimension to use (1=rows,2=columns)
#   a function with 1+ parameters (the list element, 0 or more parameters)
#   0 or more parameters also passed to the function

testf <- function(paraml,paramn){
  val = paraml[which(paramn=="v2")]
  c(sum(paraml),mean(paraml),val)
}

###################################
# each column will have the sum and mean calculated
# and the value of the paramter with name "v2"
odata = apply(pdata,2,testf,pname)

Calc  = c("Sum","Mean","Special")

xdata = cbind(Calc,odata)


xfile = file_name(myDir,testDir,outFile)
message(" >>> Generating: Output File:  ",xfile)
write.table(xdata, file = xfile, sep = ",", col.names = T, row.names =F) #save to csv

blank_line()
message(">>>>>>>>>> Complete: ",progname,"  --  Time: ",Sys.time())




