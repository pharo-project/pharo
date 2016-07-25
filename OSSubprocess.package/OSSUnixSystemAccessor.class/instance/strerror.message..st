errors
strerror: aReturnValue message: aMessage
	"Get the string description of the error numnber aReturnValue and signal 
	and error with such a description together with aMessage string"
	| internalError | 
	internalError := self primitiveStrerror: aReturnValue.
	self error: 'Error: ', aMessage, ', code: ', aReturnValue asString, ', description: ', internalError