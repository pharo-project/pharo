initialization
initializeNamed: aString in: aBrowser 
	self initialize.
	name := aString ifNil: [ self defaultName ].
	browser := aBrowser