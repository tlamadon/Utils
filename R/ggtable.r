# library to create tables in R
# this was inspired by ggplot

# setwd('~/git/ggtable')

# the standard is that if the observation is missing
# for a combination, we will put an empty cell

# as in the reshape package the philisophy is that you have 
# measured things and id things,and I guess this has to be clear from the begining

# in a table you will want to display values (the aggregation could have been done before)
# the clo/row grouping should be done according to factors, the content of each cell will
# filled with the column value


### REDUCED DOCUMENTATION ###
# RULE
# the rule should be rowcategory1 + rowcategory2 + rowcategory3 ~ colcategory1 + colcategory2
# CELL STYLE - GEOM & AES
# this should go into the aes with color/font/value/sd and will depend on the the geom use to 
# render the cell
# ROW / COLS STYLES 
# you should be able to do + topspace(c(rowcategory1)) to add a topspace when the category changes
# DATA
# the data should a long data.frame where each line contains the info for a given row ( mean / sd for example)
# SAPNNING?
# we could think of using a bool which set to one will span across all followin NAs
# COMBINING?
# it sometimes nice to allow to concat 2 tables that might not have the same geom
# pr maybe we should allow for a geom per row? or per indicator?

# obviously this could be combined with the package reshape to aggregate however you want firt

# example 1: 
#  I have a list of estimates for each combination of models and subject
#  I want to create a table where I have the subjects in columns and the models
#  and paramters in lines 
#  I also want to decorate estimates with standard errors! (we'll see after) 

# for grouping in trees for columns and rows, we create an interaction
# and remove unused interactions

# here we want each model/estimates likelihoods and then the esperiment params

#gt <- ggtable(ests, aes(rows=c("model","param") , cols = subject , color = value<0.1))          

# the data should like 
                                           
# name   , subject , value  ,  model,  param 
# k      , 1       , 0.0124 ,  hyp  ,   true 
# beta   , 1       , 1.1    ,  hyp  ,   true 
# k      , 1       , 0.0124 ,  exp  ,   true 
# beta   , 1       , 1.1    ,  exp  ,   true 
# likeli , 1       , -5     ,  hyp  ,   false 
# likeli , 1       , -5     ,  exp  ,   false 

# Ideal syntax
# ggtable(data, r=c("model,"param"), c=c("subjectid)) 
#      + hline("model") 
#      + cline("subjectid")
#      + se("se")

#data = read.csv('ex_Data.r',sep=',',)

library(gdata)
ggtable2 <- function(data , rows, cols, file=NA,view=FALSE,resize=1) {
	
  # I have to create the tree structure, some parts might be empty
  # I have a list, of variables
  # take only the different interactions, that defines the columns

  # the different columns are the existing interaction
  # between rows
  data$colval = interaction(data[,rev(cols)])
  data$rowval = interaction(data[,rev(rows)])
  colvals = levels( drop.levels(data$colval,reorder=FALSE));
  rowvals = levels( drop.levels(data$rowval,reorder=FALSE));


  # seting pval to NA is no such columns
  if (!('pval' %in% names(data))) {
    data$pval=NA
  }
  
  # --------------------------------------------------   
  #                 TABLE BODY HEADER
  # --------------------------------------------------   
  
  l = " r@{}l "; form = paste(l[seq(1,1,length.out=length(colvals))], collapse="");
  HEADER_STR = paste("\\begin{tabular}{r ", form , "} \r\n");    
  HEADER_STR = paste(  HEADER_STR , " \\toprule \r\n");    

  for (col in colvals) {
    HEADER_STR = paste(HEADER_STR , " & \\multicolumn{2}{c}{ ",   gsub('_',' ',col) , "}");
  }  
  HEADER_STR = paste(  HEADER_STR , " \\\\ \r\n");    
  #HEADER_STR = paste(  HEADER_STR , " \\cmidrule(r){2-", 2*length(colvals)+1 ,"} \r\n");    
  HEADER_STR = paste(  HEADER_STR , " \\midrule \r\n");

  # --------------------------------------------------   
  #               TABLE BODY ARRAY
  # --------------------------------------------------   
  BODY   = array("",c(length(rowvals),length(colvals)))
  rownames(BODY)  = rowvals; colnames(BODY)  = colvals
  BODYSD = array("",c(length(rowvals),length(colvals)))
  rownames(BODYSD)  = rowvals; colnames(BODYSD)  = colvals
  BODYSIG = array("",c(length(rowvals),length(colvals)))
  rownames(BODYSIG)  = rowvals; colnames(BODYSIG)  = colvals
  ROWGRPNAME = array("",c(length(rowvals),1))
  rownames(ROWGRPNAME)  = rowvals;
  ROWNAME = array("",c(length(rowvals),1))
  rownames(ROWNAME)  = rowvals;

  # ---- FOR EACH ROW -----
  for (row in rowvals) {
  
    # GET THE ROW NAME
    ldr = subset(data, rowval == row)
    ROWGRPNAME[row,1]  = paste(ldr[1,rows[1:(length(rows)-1)]], collapse=" ")
    ROWNAME[row,1]     = paste(ldr[1,rows[length(rows)]])
  
    # ---- FOR EACH COL, PROCESS THE CELL FROM THE DATA -----
    for (col in colvals) {
      
      # get the data for the cell
      ld = subset(data, rowval == row & colval == col)

      # DEFAULT
      BODYSD[row,col]  = paste("");
      BODY[row,col]    = paste(" -- ");
      BODYSIG[row,col] = paste("");

      if (length(ld[,1]) == 0) {
        next
      }
  
      if (is.na(ld$sd[1]) == FALSE)
        BODYSD[row,col] = paste("{ \\scriptsize (" , prettyNum(ld$sd,digit=3) , ")}", sep=""); 
        
      if (is.na(ld$pval[1]) == FALSE) {
        sig = "";
        if (ld$pval[1]<0.1) sig = paste(sig,"*",sep="");
        if (ld$pval[1]<0.05) sig = paste(sig,"*",sep="");
        if (ld$pval[1]<0.01) sig = paste(sig,"*",sep="");
        BODYSIG[row,col] = sig;
      }

      BODY[row,col] = paste(prettyNum(ld$value[1],digit=3));
    }
  }    
    
  # --------------------------------------------------   
  #            CONCAT TABLE BODY ARRAY
  # --------------------------------------------------   
  TABLE_BODY_STR = "";
  rlevs = levels(data[,rows[length(rows)]])
  
  # ---- FOR EACH ROW -----
  PREVROWGRPNAME = NA;
  for (row in rowvals) {

    # ADD THE GROUP NAME IF IT CHANGED
    if (is.na(PREVROWGRPNAME) | PREVROWGRPNAME != ROWGRPNAME[row,1]) {
      TABLE_BODY_STR = paste(TABLE_BODY_STR,"\\multicolumn{4}{l}{ \\bf", gsub('_',' ',ROWGRPNAME[row,1]),"} \\\\ \r\n")
      PREVROWGRPNAME = ROWGRPNAME[row,1]
    }
    
    # ADD THE ROW NAME
    TABLE_BODY_STR = paste(TABLE_BODY_STR,"\\emph{\\small ",  gsub('_',' ',ROWNAME[row,1]),"} & ")

    # ADD THE VALUE LINE
    TABLE_BODY_STR = paste(TABLE_BODY_STR,  paste(rbind(BODY[row,] , BODYSIG[row,]), collapse=" & "))
  
    # ADD THE SD LINE IF ANY
    if (any(BODYSD[row,] != "")) {
      TABLE_BODY_STR = paste(TABLE_BODY_STR," \\\\[-4pt] \r\n")
      TABLE_BODY_STR = paste(TABLE_BODY_STR," & ")
      TABLE_BODY_STR = paste(TABLE_BODY_STR,  paste(BODYSD[row,], collapse=" & & "))
      TABLE_BODY_STR = paste(TABLE_BODY_STR," \\\\[1pt] \r\n")
    } else {
      TABLE_BODY_STR = paste(TABLE_BODY_STR," \\\\[1pt] \r\n") #no sd so no negative spacing
    }
  }
  
  # --------------------------------------------------   
  #            CONCAT TABLE BODY ARRAY
  # --------------------------------------------------   
    
  TABLE_FOOTER_STR = paste("\\bottomrule \r\n")
  TABLE_FOOTER_STR = paste(TABLE_FOOTER_STR , "\\end{tabular} \r\n")
  
  if (resize!=1) {
    HEADER_STR =  paste("\\scalebox{", resize, "}{ \r\n",HEADER_STR)
    TABLE_FOOTER_STR =  paste(TABLE_FOOTER_STR , " \r\n } \r\n")
  }

  if (!is.na(file)) {
    cat(paste(HEADER_STR , TABLE_BODY_STR,TABLE_FOOTER_STR),file= file)
  } else {
    file = 'tmp'
  }

  if (view==TRUE) {
    HEADER_STR =  paste("\\documentclass[12pt]{article}  \r\n \\usepackage{booktabs}\r\n \\usepackage{fullpage}  \r\n \\usepackage{booktabs}\r\n \\usepackage{graphicx} \r\n \\begin{document} \r\n",HEADER_STR)
    TABLE_FOOTER_STR =  paste(TABLE_FOOTER_STR , " \r\n \\end{document} \r\n")
    cat(paste(HEADER_STR , TABLE_BODY_STR,TABLE_FOOTER_STR),file= paste(file,'.tex',sep=''))
    system(paste('pdflatex ', file,  '.tex' ,sep=''))
    system(paste('open ', file,  '.pdf' ,sep=''))
  }
  
  return()
}
