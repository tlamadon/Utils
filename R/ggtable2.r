# new version of ggtable

# ability to add cell style
# + ggt_cell(textcolor=var1,
# ability to add padding
# + ggt_border(padding, dotted=, dline=, sline=)
# ability to add orders and sorting
# + ggt_order(
# ability to add rows/columns
# add table options
# ggt_options(caption= , note = ,resize= , flip=)

# a class ggtable will have a list of rows and a list of column
# selector, then it will have a list of data set with each 
# different cell processors that will create the content
# of the cell

# print.ggtable will go through the list of cells and process them


# one important bit is the cell renderer, it will take a list of values
# and create the cell content, the problem is that the cell representation
# might include several cols/rows to have parenthesis and star/daggers
# for example

# step 1, for each ggt_cell a data.frame is created with id the combination of
# of rows and cols and the content of the cell. For now we will force 
# the cells to be 2x2 and so we should have also this as an id

#taes <- function (x, y, ...) 
#{
#    aes <- structure(as.list(match.call()[-1]), class = "uneval")
#    return(aes)
#}

ggt_cell_plain <- function(data=NA,desc=list(value='value')) {
 format <- function(ids,pdesc=NA,pdata=data) {
  
   # local argument overide the one from ggtable 
   if ( any(is.na(desc)) ) desc = pdesc;
   if ( any(is.na(data)) ) data = pdata;

   # here we are just going to paste the value in the
   # upper left quadrant of the cell tile
   return(ddply(data, ids, function(d) { 
       d2 = expand.grid(x=1:2,y=1:2,value='',hasValue=FALSE)
       d2$value       = as.character(d2$value)
       d2$value[1]    = paste(d[1,desc$value])
       d2$hasValue[1] = TRUE
       return(d2)
   }))
  }
  class(format) <- 'ggt_cell'
  return(format)
}


ggt_lines <- function(varlist) {}

ggt_rename <- function() {}

# this gives an id variable and a list of values
# that specifies the order in which they should appear
ggt_order <- function(varname, orderList) {

  res = list()
  res$varname = varname
  res$orders  = orderList

  class(res) <- 'ggt_order'
  return(res)
}

# for ggtable you only give a formula
# the data is passed to the cell
ggtable <- function(formula,verbose=FALSE) {
  
  # parsing the formula
  if (is.formula(formula)) 
        formula <- deparse(formula)
  if (!is.character(formula)) 
        formula <- as.character(formula)

  v = all.vars.character(formula)

  gg1  = list()
  gg1$orders = list()

  gg1$rows = v$m[[1]]
  gg1$cols = v$m[[2]]

  gg1$cells = data.frame()

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

  # if b is of type cell, we add it to the list of cells
  # to the ggtable

  # if b is an option, we add it to the list of options
  return(ggt)
}


# this function returns a data.frame with values applying
# the order given in orders -- this is used to compute
# values that go into columns and rows
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
  multv = nrow(dtmp)
  for ( v in rev(vars)) {
    if ( v %in% names(orders)) {
      od = orders[v][[1]]
      # for each value, we set the actual order
      i = 1
      for (val in rev(od)) {
        if (is.factor(dtmp[,v]  )) {
          I = levels(dtmp[,v])[dtmp[,v]] == val
        } else {
          I = dtmp[,v] == val
        }
        dtmp$my_order__[I] = dtmp$my_order__[I] + i * multv
        i = i+1
      }
    }
     multv = multv * nrow(dtmp)
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

print.ggtable <- function(ggt) {
  
  cdata = ggt$cells

  cdata = data.table(cdata)
  key(cdata) <- c(ggt$rows,ggt$cols)
  # we need to find the list of column and row values
  # they are the interactions between the values
  # of the variavle given in ids

  # we get the structure for the rows and cols
  rowframe = ggt_getIDLevels(cdata,ggt$rows,ggt$orders)
  colframe = ggt_getIDLevels(cdata,ggt$cols,ggt$orders)
  
  # next we generate the headers for the columns
  

  # we generate the body
  # we go through each row/col combination
  # get values from cdata and concat it!
  for (ro in 1:nrow(rowframe)) {
    for (co in 1:nrow(colframe)) {
      
      # get the line in cdata  that corresponds to the value
      T = data.table(c(rowframe[ro,],colframe[co,])) 
        
      



    }
  }








}



# EXAMPLE

ggt <- ggtable(treatment ~ variable) + ggt_cell_plain(mm,list(value='Estimate')) + 
       ggt_order('treatment',c('1','2'))

print(ggt)
