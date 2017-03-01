accessing
callbackReturnOn: callbackContext for: anObject
	"Void returns does not set anything"
	self isPointer ifFalse: [ ^ nil ]. 
	^ callbackContext wordResult: anObject asInteger.
