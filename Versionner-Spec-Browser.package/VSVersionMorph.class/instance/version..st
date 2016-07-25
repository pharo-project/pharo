accessing
version: anObject
	version := anObject.
	version ifNotNil: [ self build ].