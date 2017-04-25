private
haltIfCallChain: haltSenderContext contains: aSelector
	| cntxt |
	cntxt := haltSenderContext.
	[ cntxt isNil ] whileFalse: [ 
		cntxt selector = aSelector ifTrue: [ self signalIn: haltSenderContext ].
		cntxt := cntxt sender ]