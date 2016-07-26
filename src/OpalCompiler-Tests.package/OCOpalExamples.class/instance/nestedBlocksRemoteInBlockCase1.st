examples
nestedBlocksRemoteInBlockCase1
	| block |
	block := [ | a b |
		[ a := b ].
		[ b := a ] ]