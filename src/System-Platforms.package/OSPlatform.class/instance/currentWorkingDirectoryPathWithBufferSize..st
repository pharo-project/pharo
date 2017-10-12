accessing
currentWorkingDirectoryPathWithBufferSize: aBufferSize
	| buffer |
	<primitive: 'primitiveGetCurrentWorkingDirectory' module: '' error: ec>
	ec ifNotNil: [
		"Should signal the reason but the API does not accept it"
		self primitiveFail ].

	buffer := String new: aBufferSize.
	^ self getPwdViaFFI: buffer size: aBufferSize