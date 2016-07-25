tests pointer
testByteArrayToExternalAddress
	| result dest |
	

	result := self 
		primFromByteArray: 'Hello, World' asByteArray  
		toExternalAddress: (dest := ExternalAddress allocate: 12) autoRelease 
		size: 12.
		
	self assert: (dest copyFrom: 1 to: 12) asString = 'Hello, World'.
	