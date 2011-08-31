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

mappend <- function(d,variable,value) {
  non_cst_var = c()
  for (v in names(d)) {
    if ( length(unique(d[,v]))>1) non_cst_var = c(non_cst_var,v);
  }

  new_line = d[1,]
  new_line[,non_cst_var] = NA
  new_line$value[1] = value
  new_line$variable = variable
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
renameany <-function(d,rules) {

  #1) we rename the columns
  d = rename(d,rules)

  #2) try to rename the levels in the variables
  for ( n in names(rules)) {
    for ( c in colnames(d)) {
      
      # first check if the colum is string
      if ( is.character(d[1,c]) ) {
       # try to select all rows that have value n
       d[ d[,c] == n , c] = rules[n]
      }

      # for factors
      if (is.factor(d[,c])) {
        levels( d[,c]  )[levels(d[,c])==n] <- rules[n]
      }
    }
  }

  return(d)
}
