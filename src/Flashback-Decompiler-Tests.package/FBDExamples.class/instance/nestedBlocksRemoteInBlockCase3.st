examples
nestedBlocksRemoteInBlockCase3
	| block |
	self flag: 'Not well decompiled'.
	block := [ 
		[ |a| block := [ a := 1] ].
		[ |b| block := [ b := 2] ] ].