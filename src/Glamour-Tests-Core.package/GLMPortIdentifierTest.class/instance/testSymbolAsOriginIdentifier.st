tests
testSymbolAsOriginIdentifier
	self assert: (#pane asGlamourOriginIdentifier isKindOf: GLMPortIdentifier).
	self assert: #pane asGlamourOriginIdentifier paneName equals: #pane.
	self assert: #pane asGlamourOriginIdentifier portName equals: #selection