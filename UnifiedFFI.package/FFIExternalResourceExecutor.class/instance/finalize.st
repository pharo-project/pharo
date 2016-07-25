finalizing
finalize 
	session = Smalltalk session ifFalse: [ ^ self ].		
	resourceClass finalizeResourceData: data