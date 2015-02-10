
pathsFromAtoB <- function(...){
   	inner <- function(from,to,M){
		out <- list()
		if(sum(M[,from]) == 0)# the fast path out
			return(out)
		for(i in 1:nrow(M)){
			if(i == from)
				next # no infinte loops please
			if(!M[i,from])
				next
			if(i == to & M[to,from]){
				out <- c(out,list(c(from,to)))
				next
			}
			for(subpath in pathsFromAtoB(from=i,to=to,M=M))
				out <- c(out,list(c(from,subpath)))
		}
		return(out)
	}
	out <- tryCatch(inner(...),
			 error = function(e){
				 if(inherits(e,'simpleError') && grepl('infinite recursion',e$message)){
					 return(NULL)
				 }else {
					 stop("ERROR: grepl('infinite recursion',e$message)")
					 print("ERROR: grepl('infinite recursion',e$message)")
					 while(T)browser()
				 }
			 })
	if(is.null(out)){
		stop('pathsFromAtoB(...) failed due to infinite recursion.  \n\tPlease check that M does not have loops (such as diagonal elts)')
		browser()
	}
	return(out)
}

#generate the list of paths between nodes a and b from a given transition matrix
alt_pathsFromAtoB <- function(from,to,M)
	# this is a bottom-up approach instead of a top-down, which is faster
	# for directed graphs with no loops...
	lapply(pathsFromAtoB(to,from,t(M)),rev)

