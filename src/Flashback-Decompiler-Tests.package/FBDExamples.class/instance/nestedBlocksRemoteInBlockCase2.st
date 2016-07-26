examples
nestedBlocksRemoteInBlockCase2
	| block |
	self flag: 'Not well decompiled'.
	block := [ 
		[ |a| [ a := 1] ].
		[ |b| [ b := 2] ] ]