accessing
major: anObject
 
	| majorNumber |
	majorNumber := anObject asInteger.
	majorNumber ifNil: [ self invalidMajorVersion: anObject ].
	major := anObject