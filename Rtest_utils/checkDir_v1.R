progname = "cor_sites_v1"
message(">>>>>>>>>> Create: ",progname,"  --  Time: ",Sys.time())

message(" >>> Checking Root Directories <<< ")

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

message(" >>> Root Dir = ",root_dir())

message(">>>>>>>>>> Complete: ",progname,"  --  Time: ",Sys.time())




