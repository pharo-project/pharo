special context access
exceptionHandlerBlock
	"handlercontext only. access temporaries from BlockClosure>>#on:do:"
	^self tempAt: 2
