accessing
home 
	"Answer the context in which the receiver was defined."

	closureOrNil == nil ifTrue:
		[^self].
	^closureOrNil outerContext home