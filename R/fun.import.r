# better sourcing

# import just sources an R file into an environment
# the best would probably be the following:

# import(folder1.folder2.package) 
# folder1.folder2.package$fucntion1()
# 
# import(folder1.folder2.package.*)
# function1()

# import(folder1.folder2.package | pac)
# pac$function1()

# also you should be able to define some shortcuts
# like git for your git fodler
# import(git.folder1.util | util)
# will import files from git as defined in some configuration

# look for all file names starting with same string
# also look for possible folders like in git folder

# also give it a class, look in the environment for
# this file already loaded and return it, and realod first
# this prevents multiple loadings


# DOC
# ===

# import( com.toto.bidule) will just source the function found anywhere
# import( bid ~ com.toto.bidule ) will put it in a bidule environment
# import( ~ com.toto.bidule ) will put it in a com.toto.bidule environment
# import( com.toto.bidule )   will load com.bidule.r or com/toto/bidule.r
# you can define shortcuts for import like git will look into ~/git
# so import( git.utils.bidule) will import ~/git/utils/bidule.r 

require(stringr)

importenv <-setClass("importenv", contains = "environment",
                representation(source ="character"))

IMPORT_DIR_LIST <- list(git='~/git/',svn='~/gitsvn/',utils='~/git/Utils/R/')

import <- function(importformula) {
  ff <- substitute(importformula)

  # parse arguments
  if (length(ff)==3) {
    asname <- paste(ff[[2]])
    name   <- paste(ff[[3]])
    if (asname == '.') asname = name;
  } 
  if (length(ff)==2) {
    name   <- paste(ff[[2]])
    asname = name
  } 
  if (length(ff)==1) {
    name <- paste(ff)
    asname = ''
  }

  # getting file to import
  filename <- import.find(name)
  if (is.null(filename)) stop("Can't find import")

  if (str_length(asname)>0) {
    lenv =NULL
    ll = import.ls(parent.frame() )
    for (impts in ll) { if (impts@source == name) lenv=impts; }
    if (is.null(lenv)) lenv <- new("importenv",  source = name);
  } else {
    lenv = parent.frame()
  }

  source(filename,local=lenv,chdir=TRUE)
  penv = parent.env(environment())

  if (str_length(asname)>0) {  
    assign(asname,lenv,penv)
  }
}

import.find.folder <- function(pname,dir='.') {
  # first we get the list of all possible files in the current folder
  file_list = list.files(dir,include.dirs=TRUE,recursive=TRUE)
  # select only r files
  I = str_detect(file_list,'\\.[rR]$')
  file_list = file_list[I]
  name_list = str_replace(file_list,'/','.')
  name_list = str_replace(name_list,'\\.r$','')

  I = which(name_list == pname)
  if (length(I)>0) return(file_list[[I[1]]]);
  return(NULL)
}


import.find <- function(pname) {

  import_file = import.find.folder(pname)

  if (is.null(import_file) & str_detect(pname,'\\.') ) {
    # extract the first part of the name and see 
    # if we have it on our list of fodlers
    group = str_split(pname,'\\.')[[1]][1]
    pname = str_replace(pname,paste(group,'\\.',sep=''),'')

    if (group %in% names(IMPORT_DIR_LIST)) {
      import_file = import.find.folder(pname,IMPORT_DIR_LIST[[group]])
      if (!is.null(import_file)) import_file = paste(IMPORT_DIR_LIST[[group]],import_file,sep='');
    }
  }

  return(import_file)
}

# list all packages
import.ls <- function(env =parent.frame() ) {

  ll = c();
  lln = c();  
  tt = objects(envir = env)
  for (oon in tt) {
      oo <- get(oon, envir = env)
      if (class(oo)[[1]]=='importenv') {  ll = c(ll,oo); lln = c(lln,oon) }
  }
  return(ll)
}

print.importenv <- function(env) {
  cat(env@source)
}
