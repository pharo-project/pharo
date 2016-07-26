accessing - compatibility
errorCode
	self requestor ifNil: [
		^ self node methodNode asString ].
	^ self requestor contents