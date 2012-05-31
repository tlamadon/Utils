# new version of ggtable

# this is a priliminary version of a package that would like
# to be the analogous of ggplot for latex tables
# you can contact me at thibaut.lamadon@gmail.com

# ability to add cell style
# + ggt_cell(textcolor=var1,
# add table options
# ggt_options(caption= , note = ,resize= , flip=)


# TODOS
# - sideway / resize option / center
# - dictionary option
# - vertical line
# - color background
# - title / label
# - row groups titles
# - render latex in titles / remove underscores / escape

require(reshape)
require(data.table)
require(gdata)

model2frame <-function(models) {

  if (class(models)!='list') {
    models = list(models)
  }

  r=data.frame()
  i=0

  for (m in models) {
    # adding the coeffs
    if (class(m)=='mer') {
      coefs = summary(m)@coefs
    } else {
      coefs = summary(m)$coef
    }

    if (ncol(coefs)>=4) {
       ld = data.frame(coefs[,c(1,2,4)])
    } else {
       ld = data.frame(value = coefs[,1], sd = coefs[,2] ,pval=rep(NA,nrow(coefs)))
    }
    colnames(ld) <- c('value','sd','pval')
    ld$variable = rownames(coefs)
    rownames(ld) <- NULL

    # adding a BIC/AIC
    ld = rbind(ld, data.frame(variable = 'BIC', value = BIC(m),sd=NA,pval=NA ))

    # try to get rsquare
    sfit = summary(m)
    if ( 'r.squared' %in% names(sfit)) {
      ld = rbind(ld, data.frame(variable = 'Rsq', value = sfit$r.squared,sd=NA,pval=NA ))
    }

    ld$model=i                          
    r =rbind(r,ld)
    i =i+1
  }

  return(r)
}



taes <- function (x, y, ...) 
{
    aes <- structure(as.list(match.call()[-1]), class = "uneval")
    return(aes)
}

ggt_labeller <- function(str,ll) {
  str = gsub('_',' ',paste(str))
  return(str)
}

ggt_cell_plain <- function(data=NA,desc=list(value='value')) {
 format <- function(ids,pdesc=NA,pdata=data) {
  
   # local argument overide the one from ggtable 
   if ( any(is.na(desc)) ) desc = pdesc;
   if ( any(is.na(data)) ) data = pdata;

   # here we are just going to paste the value in the
   # upper left quadrant of the cell tile
   return(ddply(data, ids, function(d) { 
       d2 = expand.grid(x=1:2,y=1:2,value='',hasValue=FALSE)
       d2$spanRight   = FALSE
       d2$value       = as.character(d2$value)
       d2$value[1]    = paste('\\multicolumn{2}{c}{',round(d[1,desc$value],2),'}',sep='')
       d2$hasValue[1] = TRUE
       d2$spanRight   = TRUE
       return(d2)
   }))
  }
  class(format) <- 'ggt_cell'
  return(format)
}

ggt_note <- function(str) {

}

ggt_cell_regression <- function(data=NA,desc=list(value='Estimate',sd='Std..Error',pval='Pr...t..')) {
 format <- function(ids,pdesc=NA,pdata=data) {
  
   # local argument overide the one from ggtable 
   if ( any(is.na(desc)) ) desc = pdesc;
   if ( any(is.na(data)) ) data = pdata;

   # here we are just going to paste the value in the
   # upper left quadrant of the cell tile
   return(ddply(data, ids, function(d) { 
       d2 = expand.grid(x=1:2,y=1:2,value='',hasValue=FALSE)
       d2$value       = as.character(d2$value)
       d2$spanRight   = FALSE

       d2$value[1]    = paste(prettyNum(d[1,desc$value],digit=3))
       d2$hasValue[1] = TRUE

        if ((length(desc$pval)>0) & (desc$pval %in% names(d)))
           if (!is.na(d[1,desc$pval]))   
             if ( d[1,desc$pval] < 0.05) {
         d2$value[3]    = '*'
         d2$hasValue[3] = TRUE
       }

       if ( ! is.na(d[1,desc$sd])) {
         d2$value[2]    = paste('{\\scriptsize (' , prettyNum(d[1,desc$sd],digit=3), ')}',sep='')
         d2$hasValue[2] = TRUE
       } else {
         d2$value[2] = '--'
       }

       return(d2)
   }))
  }
  class(format) <- 'ggt_cell'
  return(format)
}

ggt_opts <- function(ll) {

}


# there are 2 ways to specify lines
# 1) you give an id var and the line will be added
# whenever the value of that id changes ( in columns or lines)
# 2) you give the value of an id var, then the line will be added
# whenever this value appears
# baasically the idvarvalue says only add the line after that 
# given value

ggt_line <- function(var, values = c(), type='|') {

  res= list()
  res$var = var
  res$vals = values
  res$type = type

  class(res) <- 'ggt_line'
  return(res)
}

ggt_rename <- function(ll = list()) {
  class(ll) <- 'ggt_rename'
  return(ll)
}

# this gives an id variable and a list of values
# that specifies the order in which they should appear
ggt_order <- function(varname, orderList) {

  res = list()
  res$varname = varname
  res$orders  = orderList

  class(res) <- 'ggt_order'
  return(res)
}

ggt_valsep <- function(var,val, type='line', level='2') {
  res = list()
  res$var   = var
  res$val   = val
  res$type  = type
  res$level = level
  class(res) <- ggt_valsep
  return(res)
}

# for ggtable you only give a formula
# the data is passed to the cell
ggtable <- function(form,data=data.frame(),verbose=FALSE) {
  
  # parsing the formula
  #if (is.formula(formula)) 
  #      formula <- deparse(formula)
  #if (!is.character(formula)) 
  #      formula <- as.character(formula)

  #v = all.vars.character(formula)

  gg1          = list()
  gg1$orders   = list()
  gg1$lineseps = c()
  gg1$maindata = data
  gg1$params   = list(resize=1,sideway=FALSE)

  gg1$rows = all.vars(form[[2]])
  gg1$cols = all.vars(form[[3]])

  gg1$cells = data.frame()
  gg1$rename = list()

  class(gg1) <- 'ggtable'
  return(gg1)
}

"+.ggtable" <- function(ggt,argb) {
  
  # add cells to the table
  if (class(argb) == 'ggt_cell') {
    # we process the cells per rows/cols using the ggt_cell and store them
    # in our list of cells
    ggt$cells = rbind(ggt$cells, argb(c(ggt$rows,ggt$cols)))
  }

  # add orders
  if (class(argb) == 'ggt_order') {
    LL = list(argb$orders)
    names(LL)<- argb$varname
    ggt$orders = c(LL, ggt$orders)
  }

  # add lines
  if (class(argb) == 'ggt_line') {
    ggt$lineseps[[ length(ggt$linseps) +1  ]] = argb
  }

  # add rename
  if (class(argb) == 'ggt_rename') {
    ggt$rename = argb
  }

  # if b is of type cell, we add it to the list of cells
  # to the ggtable

  # if b is an option, we add it to the list of options
  return(ggt)
}


# this function returns a data.frame with values applying
# the order given in orders -- this is used to compute
# values that go into columns and rows
# the rows are first stratified by the values of 
# the variable in order -- within that they get ranked
# using the supplied order
ggt_getIDLevels <- function(cdata, vars, orders=list()) {
 
  # we start by collecting the relevant columns
  # and removing duplicates
  dtmp = data.frame(cdata)
  dtmp$my_order__= 0 
  dtmp = unique(dtmp[,c(vars,'my_order__')])

  # for each variable, we try to get the order for that variable
  # if there is one, we apply the value in the list to the given value
  # then we give 
  # we are going to compute the value of each row and then sort it! 
  N = nrow(dtmp)
  multv = N

  # for each selector in reverse order 
  for ( v in rev(vars)) {
    od = orders[v][[1]]                                # we get the corresponding order
    vals = union(od,setdiff(unique(dtmp[,v]),od)) # combine order with values
    
    # go through vals in reverse order and add the index value
    i = 1
    for (val in rev(vals)) {
      if (is.factor(dtmp[,v]  )) {
        I = levels(dtmp[,v])[dtmp[,v]] == val
      } else {
        I = dtmp[,v] == val
      }
      dtmp$my_order__[I] = dtmp$my_order__[I] + i * multv
      i = i+1
    }
    multv = multv * N
  }

  dtmp = dtmp[rev(order(dtmp$my_order__)),c(vars,'my_order__')]
  dtmp$my_order__<-NULL                                          
  return(dtmp)
}

# return a Index with true for the rows where
# cdata equals each value in selector
ggt_computeSelector <- function(cdata,selector) {
  I = cdata[,1] == cdata[,1]
  for ( colname in colnames(selector)) {
    if (is.factor(cdata[,colname])) {
      I = I & (levels(cdata[,colname])[cdata[,colname]] ==
               levels(selector[,colname])[selector[1,colname]])
    } else {
      I = I & (cdata[,colname] == selector[,colname])
    }
  }
  return(I)
}

ggt_computeSpan <- function(sdata,varname) {

  sdata = data.frame(sdata)
  sdata[,varname] = as.character(sdata[,varname])

  res = data.frame()
  val = sdata[1,varname]
  lastcount=1
  for (i in 1:nrow(sdata)) {
    if (val!= sdata[i,varname]) {
      res = rbind(res,data.frame(val=val,count= i - lastcount))
      val = sdata[i,varname]
      lastcount = i
    }
  }
  # append last one
  res = rbind(res,data.frame(val=val,count= i - lastcount+1))

  return(res)
}

print.ggtable <- function(ggt,file=NA,view=TRUE,verbose=FALSE) {
  
  cdata  = ggt$cells
  params = ggt$params

  cdata = data.table(cdata)
  key(cdata) <- c(ggt$rows,ggt$cols)
  # we need to find the list of column and row values
  # they are the interactions between the values
  # of the variavle given in ids

  # we get the structure for the rows and cols
  rowframe = data.table(ggt_getIDLevels(cdata,ggt$rows,ggt$orders))
  colframe = data.table(ggt_getIDLevels(cdata,ggt$cols,ggt$orders))
  
  # next we generate the headers for the columns
  
  #        BODY
  # =================

  BODY_STR = ''

  # we generate the body
  # we go through each row/col combination
  # get values from cdata and concat it!
  colframe$hasValue = FALSE
  for (ro in 1:nrow(rowframe)) {

    # adding the line category
    if ( length(ggt$rows)>1 & ((ro==1) | 
           any( data.frame(rowframe)[ro,1:(length(ggt$rows)-1)] != 
                data.frame(rowframe)[ro-1,1:(length(ggt$rows)-1)]))) {

      BODY_STR = paste(BODY_STR,"\\multicolumn{4}{l}{ \\bf", ggt_labeller(data.frame(rowframe)[ro,1],ggt$rename),"} \\\\ \n")
    }

    UPPER_LINE = ''; LOWER_LINE = '';
    UPPER_LINE_HAS_VALUE = FALSE; LOWER_LINE_HAS_VALUE = FALSE;


    # adding cells
    for (co in 1:nrow(colframe)) {
      
      # get the line in cdata  that corresponds to the value
      T = data.table(data.frame(c(rowframe[ro,],colframe[co,])))
      T$hasValue <- NULL; # removing the hasValue        

      # get the cell content
      ld = cdata[T,]
      if (is.na(ld$hasValue[1])) {
        ld = data.table(expand.grid(x=1:2,y=1:2,value='',spanRight=FALSE,hasValue=TRUE))
        ld$value = as.character(ld$value)
      }

      # put the cell together
      if (ld[x==1 & y==1]$spanRight) {
        UPPER_LINE = paste(UPPER_LINE , ' & ' , ld[x==1 & y==1]$value , sep='') 
      } else {
        UPPER_LINE = paste(UPPER_LINE , ' & ' , ld[x==1 & y==1]$value , 
                                        ' & ' , ld[x==1 & y==2]$value ,sep='') 
      }
      LOWER_LINE = paste(LOWER_LINE , ' & ' , ld[x==2 & y==1]$value , 
                                      ' & ' , ld[x==2 & y==2]$value ,sep='') 

      UPPER_LINE_HAS_VALUE = (UPPER_LINE_HAS_VALUE |  (ld[x==1 & y==1]$hasValue) |  (ld[x==1 & y==2]$hasValue))
      LOWER_LINE_HAS_VALUE = (LOWER_LINE_HAS_VALUE |  (ld[x==2 & y==1]$hasValue) |  (ld[x==2 & y==2]$hasValue))
      
      colframe$hasValue[co] = colframe$hasValue[co] | any(ld$hasValue)
    }

    # closing the lines
    if (UPPER_LINE_HAS_VALUE) {
      BODY_STR = paste(BODY_STR, '\\emph{ ' ,ggt_labeller(data.frame(rowframe)[ro,length(ggt$rows)],ggt$rename) , '}',
                                 UPPER_LINE ,sep='') 
      if ( LOWER_LINE_HAS_VALUE ) {
        BODY_STR = paste(BODY_STR,  ' \\\\[-4pt] \n ' ,LOWER_LINE ,sep='') 
      }
      BODY_STR = paste(BODY_STR,  ' \\\\[1pt] \n ' ,sep='') 
    }
    

    # adding line styles
    # first we need to find which idvars have changed on that row
    if (ro<nrow(rowframe)) {
      rtest = (rowframe[ro,] != rowframe[ro+1,])
      rtest = names(rtest[,rtest])
    } else {
      rtest=NULL
    }

    # we go through each linsep. check if the variable matches
    for ( lsep in ggt$lineseps ) {
      idvar = lsep$var
      if ( idvar %in% rtest ) {
         BODY_STR = paste(BODY_STR, '\\hline \n ',sep='') 
      } 
    }

   }

  #   TABLE HEADER
  # ================
  # we are going through each variable, and create a line for each
  # we need to span over columns where it remains constant
  # we also need to create the string that will go in the tabular
  colframe = data.frame(colframe)

  # adding colum seps -- checking each seps

  colframe$linesep = ''
  HEADER =''
  for (v in ggt$cols) {
    spaninfo = ggt_computeSpan(colframe,v)

    LINE = paste( '\\multicolumn{',  2*spaninfo$count ,'}{c}{', ggt_labeller(spaninfo$val,ggt$rename) ,'}' , collapse=' & ')
    HEADER = paste(HEADER,'&',LINE,'\\\\ \n')
  }
  
 # WRAPPING THE TABLE
 # ==================
 colformat = paste('r' ,paste(colframe$linesep,' r@{}l ', collapse =''))
 HEADER_STR = paste("\\begin{tabular}{ ", colformat , "} \n")
 HEADER_STR = paste(  HEADER_STR , " \\toprule \n")  
 HEADER_STR = paste( HEADER_STR , HEADER)
 HEADER_STR = paste(  HEADER_STR , " \\midrule \n")

 TABLE_FOOTER_STR = paste("\\bottomrule \n")
 TABLE_FOOTER_STR = paste(TABLE_FOOTER_STR , "\\end{tabular} \n")

  if (params$resize!=1) {
    HEADER_STR =  paste("\\scalebox{", params$resize, "}{ \n",HEADER_STR)
    TABLE_FOOTER_STR =  paste(TABLE_FOOTER_STR , " \n } \n")
  }

  if (params$sideway) {
    HEADER_STR =  paste("\\begin{landscape} \\begin{sidewaystable} \n",HEADER_STR)
    TABLE_FOOTER_STR =  paste(TABLE_FOOTER_STR , " \n \\end{sidewaystable} \\end{landscape} \n")
  }

 
  RESP_STR = paste(HEADER_STR , BODY_STR,TABLE_FOOTER_STR)

  if (!is.na(file)) {
    cat(paste(RESP_STR),file= paste(file,'.tex',sep=''))
  }

  if (view==TRUE) {
    file = '.ggt.tmp'
    HEADER_STR =  paste("\\documentclass[12pt]{article} \\usepackage{lscape} 
                         \\usepackage{rotating} \n \\usepackage{booktabs}\n 
                         \\usepackage{fullpage}  \n \\usepackage{booktabs}\n 
                         \\usepackage{graphicx} \n \\usepackage{arydshln} \n
                         \\begin{document} \n",HEADER_STR)
    TABLE_FOOTER_STR =  paste(TABLE_FOOTER_STR , " \n \\thispagestyle{empty} 
                              \\end{document} \n")
    cat(paste(HEADER_STR , BODY_STR,TABLE_FOOTER_STR),file= paste(file,'.tex',sep=''))
    system(paste('/usr/texbin/pdflatex ', file,  '.tex' ,sep=''),ignore.stdout=!verbose)
    system(paste('open ', file,  '.pdf' ,sep=''),ignore.stdout=!verbose)
    #system(paste('latex ' , file,  '.tex' ,sep=''))
    #system(paste('dvipng ', file,  '.dvi' ,sep=''))
    #system(paste('open '  , file,  '1.png' ,sep=''))
  } else {
    return(RESP_STR)
  }
}

# EXAMPLE
#data(french_fries)
#
## let's do 1 regression per rep
#dt = ddply(french_fries,.(rep),function(d) {
# sfit = summary(lm(potato~treatment,d))
# res1 = data.frame(sfit$coef)
# res1$varname = rownames(sfit$coef)
# res1$reg = 'potato_taste'
# sfit = summary(lm(potato~treatment,d))
# res2 = data.frame(sfit$coef)
# res2$varname = rownames(sfit$coef)
# res2$reg = 'buttery'
# return(rbind(res1,res2))
#})

#ggt <- ggtable( varname ~ reg + rep) + 
#  ggt_cell_regression(dt)+
#  ggt_order('varname',c('treatment2','treatment1')) +
#  ggt_order('variable',c('time4')) +
#  ggt_line('reg') 
#cat(print(ggt))
