Extension { #name : #SmallInteger }

{ #category : #'*UnifiedFFI' }
SmallInteger >> unpackFromArity: arity [
	"FFI can arrive here if the assigned pointer is NULL (in which case plugin will answer not an 
	 address but a plain 0... Take this as an example: 

	void function(char **errorMessage) {
		&x = NULL; //This will mean no errorMessage
	}
	
	then the corresponding Pharo method 
	
	errorMessage := ExternalAddress new. 
	self function: errorMessage.

	It will try to unpack a 0 (NULL)... and it should answer still NULL. 
	This is the only case where is valid."
	self = 0 ifFalse: [ self error: 'Should not arrive here. FFI bad code.' ].
	^ self
]
