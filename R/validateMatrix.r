
validateMatrix <- function(mx){
	# a convenience function that checks the format of transition 
	# the matrix describint all possible paths through the network.
	# Typically used in conjunction with allPossibleGates, as in: 
	# 
	# validateMatrix(allPossibleGates(switchStatesToGateMx,nSwitches))
	# 
	mx <- mx + 0
	if(! all(mx %in% 0:1) )
		stop('Transition matrix must convert to numeric values of 1 or zero')
	# must not have any loops
	if(!all(diag(mx) == 0))
		stop('mx must not have infinite loops (diagonals in this case)')
	if(any(mx & t(mx)))
		stop('mx must not have infinite loops (isimple off-diagonal loop (mx & t(mx)) in this case)')
	if(!all((mx %^% nrow(mx)) == 0))
		stop('mx must not have infinite loops (off diagonals in this case)')
	# an origin is a node with no edges pointing to it 
	origins <- apply(mx,1,sum) == 0
	if(sum(origins) < 1)
		stop('The graph must have an origin')
	if(sum(origins) > 1)
		stop('The graph must not have more than 1 origin')
	nodesTraversedTo <- origins
	for(i in 1:nrow(mx))
		nodesTraversedTo <- nodesTraversedTo | ((mx %^% i) %*% origins)
	if(any(!nodesTraversedTo)){
		stop(paste('Nodes (',
				   paste(which(!nodesTraversedTo),collapse == ', '),
				   ') cannot be traversed to from the origin'))
	}
}

