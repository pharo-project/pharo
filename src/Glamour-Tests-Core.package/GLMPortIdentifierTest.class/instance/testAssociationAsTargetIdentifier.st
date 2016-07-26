tests
testAssociationAsTargetIdentifier
	self assert: ((#pane -> #port) asGlamourTargetIdentifier isKindOf: GLMPortIdentifier).
	self assert: (#pane -> #port) asGlamourTargetIdentifier paneName equals: #pane.
	self assert: (#pane -> #port) asGlamourTargetIdentifier portName equals: #port