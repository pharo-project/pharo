examples-blocks-optimized
exampleToDoOutsideTempNotInlined
	| block temp |
	block := [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	^collection "#(5 5 5 5 5)"