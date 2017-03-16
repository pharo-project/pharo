tests
testMethodAdditionWithSourceCodeChanged

	| aClass anEntry |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	anEntry := monitor log head.

	aClass compile: 'fortyTwo ^nil'.
	
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^nil'.
	self apply: { anEntry }.
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'.
