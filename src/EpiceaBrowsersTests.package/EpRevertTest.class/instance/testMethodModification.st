tests
testMethodModification

	| aClass anEntry |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	aClass compile: 'fortyTwo ^nil'.
	anEntry := monitor log head.
	
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^nil'.
	self revert: { anEntry }.
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'.
