examples-blocks-optimized
exampleWhileWithTempNotInlined
	| index block |
	index := 0.
	block := [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	[ index < 5 ] whileTrue: block.
	^collection "#(1 2 3 4 5)"