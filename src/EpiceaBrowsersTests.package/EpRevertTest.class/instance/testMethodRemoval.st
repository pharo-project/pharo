tests
testMethodRemoval

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	self setHeadAsInputEntry.

	self assert: (inputEntry content isKindOf: EpMethodRemoval).
	self deny: (aClass includesSelector: #fortyTwo).
	self revertInputEntry.
	self assert: (aClass includesSelector: #fortyTwo).
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'.
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.
