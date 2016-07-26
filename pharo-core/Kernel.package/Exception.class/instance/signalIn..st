signaling
signalIn: context
	"Ask ContextHandlers in the sender chain starting at the given context to handle this signal. 
	The default is to execute and return my defaultAction."

	signalContext := context.
	signaler ifNil: [ signaler := self receiver ].
	^ signalContext nextHandlerContext handleSignal: self