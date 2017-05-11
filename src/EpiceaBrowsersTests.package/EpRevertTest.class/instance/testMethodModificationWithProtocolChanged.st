tests
testMethodModificationWithProtocolChanged

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'one'.
	(aClass>>#fortyTwo) protocol: 'two'.
	self setHeadAsInputEntry.

	(aClass>>#fortyTwo) protocol: 'three'.
	
	self assert: (aClass>>#fortyTwo) protocol equals: 'three'.
	self revertInputEntry.
	self assert: (aClass>>#fortyTwo) protocol equals: 'one'.
