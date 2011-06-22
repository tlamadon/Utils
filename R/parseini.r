Parse.INI <- function(INI.filename)
{
    connection <- file(INI.filename)
  Lines  <- readLines(connection)
    close(connection)

    Lines <- chartr("[]", "==", Lines)  # change section headers

      connection <- textConnection(Lines)
      d <- read.table(connection, as.is = TRUE, sep = "=", fill = TRUE)
        close(connection)

        L <- d$V1 == ""                    # location of section breaks
          d <- subset(transform(d, V3 = V2[which(L)[cumsum(L)]])[1:3],
                                                 V1 != "")

          ToParse  <- paste("INI.list$", d$V3, "$",  d$V1, " <- '",
                                                d$V2, "'", sep="")

            INI.list <- list()
            eval(parse(text=ToParse))

              return(INI.list)
}
