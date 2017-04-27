tests
testMethodModification

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	aClass compile: 'fortyTwo ^nil'.
	self setHeadAsInputEntry.
	
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^nil'.
	self revertInputEntry.
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'.
