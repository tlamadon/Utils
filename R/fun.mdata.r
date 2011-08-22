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

# this first function moves a given var fr
# this function will merge data1 with data2
# using as id variable 
# mmerge <- function(data1,data2,by,vars) {
# 
# 
#   # first we find 
#   # first we cast the data to get the byvar in columns
#   data1 = 
#   
# 
#   # the ddfun selects the vars 
#   ddfun <- function(d) {
#     # find from data2, the line that matches in byvar
#     
# 
# 
#   }
# 
#   # we run a ddply per each byid
# 
# }

# #todo deal with factors as well!
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
