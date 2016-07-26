private-exceptions
isHandlerContext
	"is this context for #on:do:?"
	^self isHandlerOrSignalingContext and: [ self selector == #on:do: ]