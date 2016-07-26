tests
testSymbolAsTargetIdentifier
	self assert: (#pane asGlamourTargetIdentifier isKindOf: GLMPortIdentifier).
	self assert: #pane asGlamourTargetIdentifier paneName equals: #pane.
	self assert: #pane asGlamourTargetIdentifier portName equals: #entity