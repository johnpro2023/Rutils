# testing sink


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

mydir = root_dir()

confile = paste(mydir,"/myconsole.txt",sep="")

zz <- file(confile, open = "wt")

message(" Test - before starting sink to",confile)

sink(zz)
sink(zz,type = "message")

#for (i in 1:10) {
#  message(" >>> Test: ",i)
#}

# message(">>>>>>>>>>>>>>>>>>>>>> Investigation IDs")
# for (iid in s_invcid) message(iid)
# message(">>>>>>>>>>>>>>>>>>>>>>")
# message(">>>>>>>>>>>>>>>>>>>>>> Investigation Dates")
# for (iid in s_invcvd) message(iid)

message(">>>>>>>>>>>>>>>>>>>>>> Begaviour_1")
for (id in sort(u_beh_1)) message(id)
message(">>>>>>>>>>>>>>>>>>>>>> Begaviour_2")
for (id in sort(u_beh_2)) message(id)
message(">>>>>>>>>>>>>>>>>>>>>>")
message(">>>>>>>>>>>>>>>>>>>>>> Prey_1")
for (id in sort(u_prey_1)) message(id)
message(">>>>>>>>>>>>>>>>>>>>>> Prey_2")
for (id in sort(u_prey_2)) message(id)


sink(type = "message")
sink()

message(" Test - stopped sink")