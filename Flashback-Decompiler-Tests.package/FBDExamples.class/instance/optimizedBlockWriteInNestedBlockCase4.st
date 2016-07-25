examples
optimizedBlockWriteInNestedBlockCase4
	| t1 |
	[(true) whileTrue: [ [ t1 < 5 ] whileTrue: [ t1 := 1 ] ] ]
