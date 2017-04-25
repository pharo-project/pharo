testing
isAvailable
	^ Smalltalk os isUnix 
		or: [ Smalltalk os isMacOS ]