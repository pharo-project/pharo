accessing
callbackReturnOn: callbackContext for: anObject
	"Void returns does not set anything"
	self isPointer 
		ifTrue: [ callbackContext wordResult: anObject ].
	^ nil