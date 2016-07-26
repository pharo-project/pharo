examples
nestedBlocksRemoteInBlockCase3
	| block |
	block := [ 
		[ |a| block := [ a := 1] ].
		[ |b| block := [ b := 2] ] ].