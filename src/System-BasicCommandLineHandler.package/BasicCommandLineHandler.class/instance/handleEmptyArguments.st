activation
handleEmptyArguments
	Smalltalk isHeadless
		ifFalse: [ ^ self ].
	
	self help.