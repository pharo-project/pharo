tests
testMethodAdditionWithSourceCodeChanged

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	self setHeadAsInputEntry.

	aClass compile: 'fortyTwo ^nil'.
	
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^nil'.
	self applyInputEntry.
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'.
