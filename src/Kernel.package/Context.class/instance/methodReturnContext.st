accessing
methodReturnContext
	"Answer the context from which an ^-return should return from."

	closureOrNil == nil ifTrue:
		[^self].
	^closureOrNil outerContext methodReturnContext