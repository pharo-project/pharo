examples
optimizedBlockWriteInNestedBlockCase3
	| t1 |
	[(true) ifTrue: [ [ t1 < 5 ] whileTrue: [ t1 := 1 ] ] ]
