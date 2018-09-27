validating
validateMajorVersion: anObject
	
	| majorNumber |
	majorNumber := anObject asInteger.
	majorNumber ifNil: [ self invalidMajorVersion: anObject ].