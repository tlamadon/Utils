# utility function for melted data

# things that tend to be painfull to do

# 1) you want to merge a data in long format based
# on a combination of values from variable and from ids...

# 2) a variable like optimal/random is present for lots
# of subcases, you want to say ... ~ subject + opt + random 
# to get the subject, then the optimal subject and the random
# subject 
# I think this is specification of a marginalization

# 3) you want to generate a new variable ( you can use ddply 
# but you need to get the value from the a data.frame, it's a
# bit annoying

# 4) combining when some columns are missing (but I guess this is a sort 
# of merge)


# I think a gcast function that would take sg like  var1 + var2 + var3 ~ var4 +var5 ....
# but where vars can be either level names or column names would be pretty awesome

# mcompute ( newvar = formula | ids ) 
# should use variable name in either col or levels, 
# if ... is given , then it should just do it by the variable that are mentioned
# basically trying to find the best set of id, the finer set of id ( risking doing it on sd as well ....)

mappend <- function(d,variable,value,.varcol='variable',.valcol='value') {
  non_cst_var = c()
  for (v in names(d)) {
    if ( length(unique(d[,v]))>1) non_cst_var = c(non_cst_var,v);
  }

  new_line = d[1,]
  new_line[,non_cst_var] = NA
  new_line[1,.valcol  ] = value
  new_line[1,.varcol  ] = variable
  d = rbind(d,new_line)

  return(d)
}

mappends <- function(d,variable,values) {
  non_cst_var = c()
  for (v in names(d)) {
    if ( length(unique(d[,v]))>1) non_cst_var = c(non_cst_var,v);
  }

  new_line = d[1,]
  new_line[,non_cst_var] = NA
  new_line$variable[1] = variable
  new_line[1,names(values)] = values 

  d = rbind(d,new_line)

  return(d)
}


# this first function moves a given var fr
# this function will merge data1 with data2
# using as id variable 
mmerge <- function(data1,data2,idvar,byvar,vars) {
  
  # 1) we find which vars are ids and which vars
  # are variables
  varid = setdiff(names(data1),c('variable','value'))

  byid       = intersect(varid, byvar)
  byvariable = setdiff(byvar, byvar) 

  # next we run a ddply per byid
  nd = ddply(data1,idvar,function(d) {

    # we need to construct a selector
    I = seq(1,l=nrow(data2))

    # select only if value if equal
    for ( v in byvariable ) {
      I = I & (data2[,v] == d$value[variable == v])
    }
    for ( v in byid ) {
      I = I & (data2[,v] == d[1,v])
    }
   
    # finally we append the value of the I row to the data frame
    for ( v in vars) {
      d = mappend(d,v,data2[I,v])
    }

    return(d)
  })

  return(nd)
}

# rename anything in the table (columns or cells)
# you can specify a subset of columns to look at
# if the selector is ambiguous
# note that you can have cicular renaming, because the data is copied first
renameany <-function(d,rules,cols=c()) {
  if (length(cols)==0) {
    cols=colnames(d)
  }

  # we use the original always
  dorig = d

  #2) try to rename the levels in the variadbles
  for ( n in names(rules)) {
    for ( c in cols) {
      
      # first check if the colum is string
      if ( is.character(dorig[1,c]) ) {
       # try to select all rows that have value n
       d[ dorig[,c] == n , c] = rules[n]
      }

      # for factors
      if (is.factor(dorig[,c])) {
        levels( d[,c]  )[levels(dorig[,c])==n] <- rules[n]
      }
    }
  }

  #1) we rename the columns
  d = rename(d,rules)


  return(d)
}



