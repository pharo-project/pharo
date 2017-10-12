accessing
currentWorkingDirectoryPathWithBufferSize: aBufferSize
	| buffer |
	<primitive: 'primitiveGetCurrentWorkingDirectory' module: '' error: ec>

	"If the primitive is not found, the error code will be the 11th: #'not found'"
	(((Smalltalk specialObjectsArray at: 52) at: 11) == ec) ifTrue: [	
		buffer := String new: aBufferSize.
		^ self getPwdViaFFI: buffer size: aBufferSize
	].

	"Should signal the reason but the API does not accept it"
	self primitiveFail
	