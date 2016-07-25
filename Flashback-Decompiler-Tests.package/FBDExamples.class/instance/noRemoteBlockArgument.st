examples
noRemoteBlockArgument
	| block block1 block2 |
	block := [ :arg | | temp | temp := arg. [ temp ] ].
	block1 := block value: 1.
	block2 := block value: 2.

