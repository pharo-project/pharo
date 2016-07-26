special context access
exceptionClass
	"handlercontext only. access temporaries from BlockClosure>>#on:do:"
	^self tempAt: 1
