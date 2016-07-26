handles
addDebugHandle: handleSpec
	self class haloWithDebugHandle ifTrue:
		[self addHandle: handleSpec
			on: #mouseDown send: #doDebug:with: to: self]
