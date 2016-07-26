system startup
startUp: isImageStarting
	isImageStarting ifFalse: [ ^ self ].
	
	self clearRegistry.
	"update session"
	Session := Smalltalk session.