tests
testMethodAdditionWithProtocolChanged

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.

	(aClass>>#fortyTwo) protocol: 'none'.
	
	self assert: (aClass>>#fortyTwo) protocol equals: 'none'.
	self applyInputEntry.
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.
