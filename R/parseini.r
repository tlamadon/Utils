read.ini <- function(filename)
{
  connection <- file(filename)
  Lines  <- readLines(connection)
  close(connection)

  Lines <- chartr("[=", "#=", Lines)  # change section headers

  res = list()

  for (l in Lines) {
    if (length(grep("\\\\n", l))==0) {
        ls = strsplit(l, "=")[[1]]
  	if( length(ls)<2) next;

	# remove all spaces 
	value = gsub(" ", "",ls[2])
	key   = gsub(" ", "",ls[1])

	# check if we have a numeric, and store it as such
	if (length(grep("[a-zA-Z\\,]", value))==0) {
	  res[key]=as.numeric(value[1])
	} else {
	  res[key]=value
	}	
    }
  }


  return(res)
}
