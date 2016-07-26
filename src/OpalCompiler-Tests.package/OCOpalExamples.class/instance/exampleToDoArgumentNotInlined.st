examples-blocks-optimized
exampleToDoArgumentNotInlined
	| block |
	block := [ :index |
		collection add: [ index ] ].
	1 to: 5 do: block.
	^collection "#(1 2 3 4 5)"