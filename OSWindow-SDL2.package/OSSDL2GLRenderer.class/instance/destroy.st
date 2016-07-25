initialize
destroy
	self onRenderThreadBlocking: [
		context ifNotNil: [ context delete ].
		context := nil.
	]
	