accessing
outerContext
	"Answer the context within which the receiver is nested."

	^closureOrNil ifNotNil:
		[closureOrNil outerContext]