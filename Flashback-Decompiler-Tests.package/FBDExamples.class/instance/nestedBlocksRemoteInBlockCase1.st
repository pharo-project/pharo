examples
nestedBlocksRemoteInBlockCase1
	| block |
	self flag: 'Not well decompiled'.
	block := [ | a b |
		[ a := b ].
		[ b := a ] ]