examples-blocks-optimized
exampleWhileWithTemp
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	^collection "#(1 2 3 4 5)"