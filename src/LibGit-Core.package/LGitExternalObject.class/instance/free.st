initialize-release
free
	handle isExternalAddress 
		ifTrue: [ 
			self class finalizeResourceData: handle.
			self assert: handle isNull ]