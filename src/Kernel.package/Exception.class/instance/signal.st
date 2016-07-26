signaling
signal
	"Ask ContextHandlers in the sender chain to handle this signal.  The default is to execute and return my defaultAction."

	signalContext := thisContext contextTag.
	signaler ifNil: [ signaler := self receiver ].
	^ signalContext nextHandlerContext handleSignal: self