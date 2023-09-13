#!/usr/bin/env Rscript

#Childhood Cancer Data Initiative - Template_ExampleR.R

#This script will take a CCDI metadata submission template file and create a fake example data set.

##################
#
# USAGE
#
##################

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CCDI-Template_ExampleR.R --help


##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("readr","dplyr","openxlsx","stringi","words","readxl","janitor","optparse","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org"))

#Load libraries.
suppressMessages(library(readr,verbose = F))
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(openxlsx, verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(words,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))

#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-t", "--template"), type="character", default=NULL, 
              help="dataset template file, CCDI_submission_metadata_template.xlsx", metavar="character"),
  make_option(c("-n", "--number"), type="integer", default=20, 
              help="number of entries to be created, default is 20")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nCCDI-Template_ExampleR v1.0.0")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$template)){
  print_help(opt_parser)
  cat("Please supply both the input file (-f) and template file (-t), CCDI_submission_metadata_template.xlsx.\n\n")
  suppressMessages(stop(call.=FALSE))
}

#Template file pathway
template_path=file_path_as_absolute(opt$template)

num_of_random=opt$number


###########
#
# File name rework
#
###########

#Rework the file path to obtain a file extension.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(template_path)),pattern = ".", n=2)[[1]][2])
ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(template_path)),pattern = ".", n=2)[[1]][1]))
path=paste(dirname(template_path),"/",sep = "")

#Output file name based on input file name and date/time stamped.
output_file=paste(file_name,
                  "_",
                  num_of_random,
                  "ExampleR",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
                  sep="")


##############
#
# Pull Dictionary for references
#
##############

#determine data dictionary
df_dict=suppressMessages(openxlsx::read.xlsx(xlsxFile = template_path,sheet = "Dictionary"))
df_dict=remove_empty(df_dict,c('rows','cols'))

#Look for all entries that have a value
all_properties=unique(df_dict$Property)[grep(pattern = '.',unique(df_dict$Property))]
#Remove all entries that are all spaces
all_properties=all_properties[!grepl(pattern = " ",x = all_properties)]
#Pull out required property groups
required_property_groups=unique(df_dict$Required[!is.na(df_dict$Required)])
required_properties=df_dict$Property[!is.na(df_dict$Required)]


################
#
# Read in TaVS page to create value checks
#
################

#Read in Terms and Value sets page to obtain the required value set names.
df_tavs=suppressMessages(openxlsx::read.xlsx(xlsxFile = template_path, sheet = "Terms and Value Sets"))
df_tavs=remove_empty(df_tavs,c('rows','cols'))

#Pull out the positions where the value set names are located
VSN=unique(df_tavs$Value.Set.Name)
VSN=VSN[!is.na(VSN)]

df_all_terms=list()

#Pull the list of values for each controlled vocabulary property.
for (VSN_indv in VSN){
  df_all_terms[[VSN_indv]] = list(filter(df_tavs,Value.Set.Name==VSN_indv)["Term"][[1]])
}

#############
#
# get_os func for later use in applying guids.
#
#############

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


###############
#
# Fake Data Generation
#
###############

#create df of filtered words to be used as fake data
filtered_words=filter(words,word_length>5, word_length<10)

#determine sheets in submission template
sheet_names=readxl::excel_sheets(path = template_path)
non_template_sheets=c("README and INSTRUCTIONS","Dictionary","Terms and Value Sets")
sheet_names=sheet_names[!sheet_names %in% non_template_sheets]

#put together a list of node data frames
workbook_list=list()

cat("\nThe data is being generated for each tab.\n\n")

#create counter for tabs
pb=txtProgressBar(min=0,max=length(sheet_names),style = 3)
x=0

for (sheet in sheet_names){
  x=x+1
  setTxtProgressBar(pb,x)
  df=openxlsx::read.xlsx(xlsxFile = template_path, sheet = sheet)
  workbook_list=append(x = workbook_list, values = list(df))
  names(workbook_list)[length(workbook_list)]<-sheet
}


#for each node in the list
for (node in sheet_names){
  #create a data frame and empty "add" data frame
  df=workbook_list[node][[1]]
  df_add=df
  #for each instance of the number of random submitted entries
  for (x in 1:num_of_random){
    for (col_num in 1:length(colnames(df))){
      col_name=colnames(df)[col_num]
      if (col_name=="type"){
        df_add[col_name]=node
      }else if (grepl(pattern = "\\.", x = col_name)){
        #Skip linking properties at this point, since they might not populate in the correct order.
        #We will link all the properties at a later time once everything is populated.
      }else{
        if (col_name %in% df_dict$Property){
          dict_val_loc=grep(pattern = TRUE,x = df_dict$Property %in% col_name)
          value_type=unique(tolower(df_dict$Type[dict_val_loc]))
          if (!is.na(value_type)){
            #for a string or an array of strings that do not allow enums
            if(value_type=="string" | (grepl(pattern = 'string', x = value_type) & !grepl(pattern = 'enum', x = value_type))){
              
              if (col_name=="md5sum"){
                #make a random string that follows md5sum regex and assign it
                random_string=stri_rand_strings(n = 1,length = 32, pattern = '[a-f0-9]')
                df_add[col_name]=random_string
              }else if (col_name=="dcf_indexd_guid"){
                #make a UUID, apply dcf guid guidelines and assign it
                if (get_os()=="osx"){
                  uuid=tolower(system(command = "uuidgen", intern = T))
                }else{
                  uuid=system(command = "uuid", intern = T)
                }
                df_add[col_name]=paste('dg.4DFC/',uuid,sep="")
              }else if (col_name=="file_url_in_cds"){
                #make a set of random words into a string and assign it
                random_words=c()
                for (y in 1:2){
                  random_word=filtered_words[,1][runif(n = 1,min = 1, max = dim(filtered_words)[1])]
                  random_words=c(random_words,random_word)
                }
                random_integer=round(x = runif(n = 1, min=0, max=9), digits = 0)
                random_words=c(random_words,random_integer)
                random_string=paste(random_words, collapse = "_")
                df_add[col_name]=paste('s3://',random_string,sep = "")
              }else{
                #make a set of random words into a string and assign it
                random_words=c()
                for (y in 1:2){
                  random_word=filtered_words[,1][runif(n = 1,min = 1, max = dim(filtered_words)[1])]
                  random_words=c(random_words,random_word)
                }
                random_integer=round(x = runif(n = 1, min=0, max=9), digits = 0)
                random_words=c(random_words,random_integer)
                random_string=paste(random_words, collapse = "_")
                df_add[col_name]=random_string
              }
              
            }else if (value_type=="integer" | grepl(pattern = 'integer', x = value_type)){
              #make a random integer and assign it
              random_integer=round(x = runif(n = 1, min=0, max=1000000), digits = 0)
              df_add[col_name]=random_integer
              
            }else if (value_type=="number" | grepl(pattern = 'number', x = value_type)){
              #make a random number and assign it
              random_number=round(x = runif(n = 1, min=0, max=1000000), digits = 2)
              df_add[col_name]=random_number
              
            }else if (value_type=="enum" | grepl(pattern = 'enum', x = value_type)){
              #choose a random permissible value and assign it
              df_tavs_prop=df_all_terms[col_name][[1]][[1]]
              random_enum=round(x = runif(n = 1, min = 1, max = length(df_tavs_prop)),digits = 0)
              enum_value=df_tavs_prop[random_enum]
              
              if (is.na(enum_value)){
                enum_value="NA"
              }
              
              df_add[col_name]=enum_value
            }
          }
        }
      }
    }
    df=rbind(df,df_add)
  }
  df=df[-1,]
  rownames(df)<-NULL
  
  #get rid of the 'id' column values, they are not required.
  df["id"]=NA
  
  #add data frame back into workbook list
  workbook_list[node]=list(df)
}

#Create some limits on higher level parent nodes as to avoid making an unrealistic submission.

#Make sure there is only one study and one study_admin
workbook_list['study'][[1]]=workbook_list['study'][[1]][1,]
workbook_list['study_admin'][[1]]=workbook_list['study_admin'][[1]][1,]

#Fix acl
if ("acl" %in% names(workbook_list['study'][[1]])){
  workbook_list['study'][[1]]['acl']=paste("['",workbook_list['study'][[1]]['acl'][[1]],"']",sep="")
}else if ("acl" %in% names(workbook_list['study_admin'][[1]])){
  workbook_list['study_admin'][[1]]['acl']=paste("['",workbook_list['study_admin'][[1]]['acl'][[1]],"']",sep="")
}else {
  cat("ACL property not found in expected node.")
}


#Limit the number of study_arms, study_funding, study_personnel, and publications.
workbook_list['study_arm'][[1]]=workbook_list['study_arm'][[1]][1:3,]
workbook_list['study_funding'][[1]]=workbook_list['study_funding'][[1]][1:5,]
workbook_list['study_personnel'][[1]]=workbook_list['study_personnel'][[1]][1:5,]
workbook_list['publication'][[1]]=workbook_list['publication'][[1]][1:3,]


###########
#
# Link nodes
#
##########


#create linking properties
for (node in sheet_names){
  #skip study node, no linking required
  if (node != "study"){
    #pull data frame
    df=workbook_list[node][[1]]
    df_cols=colnames(df)
    for (col_name in df_cols){
      if (grepl(pattern = "\\.", x = col_name)){
        #remove the [node].id columns from this as well
        if( !grepl(pattern = '\\.id', x = col_name)){
          prev_node=unlist(stri_split_fixed(str = col_name, pattern = "."))[1]
          prev_prop=unlist(stri_split_fixed(str = col_name, pattern = "."))[2]
          df_prev=workbook_list[prev_node][[1]]
          value_prev=df_prev[prev_prop][[1]]
          if(length(value_prev)<dim(df[col_name])[1]){
            fit_size=dim(df[col_name])[1]
            actual_size=length(value_prev)
            multi=trunc(fit_size/actual_size)+1
            value_prev=rep(x = value_prev, multi)
            value_prev=value_prev[1:fit_size]
          }
          df[col_name]<-value_prev
          workbook_list[node][[1]]=df
        }
      }
    }
  }
}

#Since multiple links from a child entry to different parents node is not common, we need to remove extra links when they could possibly happen.
for (node in sheet_names){
  #skip study node, no linking required
  if (node != "study"){
    #pull data frame
    df=workbook_list[node][[1]]
    df_cols=colnames(df)
    if(length(grep(pattern = "\\.", x = df_cols))>1){
      conn_val=df_cols[grep(pattern = "\\.", x = df_cols)]
      #remove the [node].id columns, we don't want to include those
      conn_val=conn_val[!grepl(pattern = "\\.id", x = conn_val)]
      conn_len=length(conn_val)
      for (x in 1:num_of_random){
        col_ran=round(runif(n = 1, min = 1, max = conn_len))
        conn_val_rans=conn_val[-col_ran]
        for (conn_val_ran in conn_val_rans){
          df[conn_val_ran][x,]<-NA
        }
      }
    }
    workbook_list[node][[1]]=df
  }
}


###############
#
# Write out
#
###############

#Write out file

cat("\nThe file is being written out at this time.\n")

wb=openxlsx::loadWorkbook(file = template_path)

pb=txtProgressBar(min=0,max=length(sheet_names),style = 3)
x=0

for (node in sheet_names){
  x=x+1
  setTxtProgressBar(pb,x)
  df=workbook_list[node][[1]]
  openxlsx::deleteData(wb, sheet = node,rows = 1:(dim(df)[1]+1),cols=1:(dim(df)[2]+1),gridExpand = TRUE)
  openxlsx::writeData(wb=wb, sheet=node, df, na.string = "NA")
  openxlsx::saveWorkbook(wb = wb,file = paste(path,output_file,'.',ext,sep = ""), overwrite = T)
}



cat(paste("\n\nProcess Complete.\n\nThe output file can be found here: ",path,"\n\n",sep = "")) 
