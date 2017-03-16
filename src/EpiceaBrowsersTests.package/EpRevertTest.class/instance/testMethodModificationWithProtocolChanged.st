tests
testMethodModificationWithProtocolChanged

	| aClass anEntry |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'one'.
	(aClass>>#fortyTwo) protocol: 'two'.
	anEntry := monitor log head.

	(aClass>>#fortyTwo) protocol: 'three'.
	
	self assert: (aClass>>#fortyTwo) protocol equals: 'three'.
	self revert: { anEntry }.
	self assert: (aClass>>#fortyTwo) protocol equals: 'one'.
