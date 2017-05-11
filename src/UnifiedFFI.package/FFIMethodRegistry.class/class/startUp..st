system startup
startUp: isImageStarting
	isImageStarting ifFalse: [ ^ self ].
	uniqueInstance ifNil: [ ^self ].
	
	self resetAll