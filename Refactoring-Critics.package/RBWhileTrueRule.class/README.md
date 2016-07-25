Checks for users of whileTrue: when the shorter to:do: would work.

For example
 
statements1. 
[index <= stop] 
	whileTrue: 
	[ 
	blockStmts1. 
	index := index + 1].
statements2