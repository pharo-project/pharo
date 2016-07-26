exceptioninstantiator
signal: message in: context
	"Signal the occurrence of an exceptional condition with a specified textual description in the given context."

	^ self new 
		messageText: message;
		signalIn: context