examples-blocks-optimized
exampleWhileModificationAfterNotInlined
	| index block |
	index := 0.
	block := [ 
		collection add: [ index ].
		index := index + 1 ].
	[ index < 5 ] whileTrue: block.
	^collection "#(5 5 5 5 5)"