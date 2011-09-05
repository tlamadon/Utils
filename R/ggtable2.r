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

taes <- function (x, y, ...) 
{
    aes <- structure(as.list(match.call()[-1]), class = "uneval")
    return(aes)
}

ggt_cell_plain <- function(data=NA,desc=list(value='value')) {
 format <- function(ids,pdesc=NA,pdata=data) {
  
   # local argument overide the one from ggtable 
   if ( any(is.na(desc)) ) desc = pdesc;
   if ( any(is.na(data)) ) data = pdata;

   # here we are just going to paste the value in the
   # upper left quadrant of the cell tile
   return(ddply(data, ids, function(d) { 
       d2 = expand.grid(x=1:2,y=1:2,value=NA)
       d2$value[1] = paste(d[1,desc$value])
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
ggt_order <- function() {}

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

  # if b is of type cell, we add it to the list of cells
  # to the ggtable

  # if b is an option, we add it to the list of options
  return(ggt)
}


ggt_getIDLevels <- function(cdata, vars, orders=list()) {
 
  # we start by collecting the relevant columns
  # and removing duplicates 
  dtmp = unique(cdata[,vars])
  dtmp$my_order__= 0 

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

  return(dtmp[rev(order(dtmp$my_order__)),vars])
}



print.ggtable <- function(ggt) {
  
  cdata = ggt$cells

  # we need to find the list of column and row values
  # they are the interactions between the values
  # of the variavle given in ids

  # we start by selecting the columns that we need, and keep unique 
  # values

  cdata$colval = interaction(data[,rev(cols)])
  cdata$rowval = interaction(data[,rev(rows)])
  colvals = levels( drop.levels(data$colval,reorder=FALSE));
  rowvals = levels( drop.levels(data$rowval,reorder=FALSE));

  # how to apply the order, given by some

}



# EXAMPLE

ggt <- ggtable(treatment ~ variable) + ggt_cell_plain(mm,list(value='Estimate')) 
print(ggt)
