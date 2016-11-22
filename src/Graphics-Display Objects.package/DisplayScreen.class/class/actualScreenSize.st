accessing
actualScreenSize
	<primitive: 106>

	^ World osWindow 
		ifNotNil: [:anOSWindow | anOSWindow extent]
		ifNil: [ self primitiveFailed ].
