tests
testAssociationAsOriginIdentifier
	self assert: ((#pane -> #port) asGlamourOriginIdentifier isKindOf: GLMPortIdentifier).
	self assert: (#pane -> #port) asGlamourOriginIdentifier paneName equals: #pane.
	self assert: (#pane -> #port) asGlamourOriginIdentifier portName equals: #port