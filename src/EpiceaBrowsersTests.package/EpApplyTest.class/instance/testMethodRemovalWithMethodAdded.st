tests
testMethodRemovalWithMethodAdded

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	self setHeadAsInputEntry.

	aClass compile: 'fortyTwo ^42' classified: 'number'.

	self assert: (inputEntry content isKindOf: EpMethodRemoval).
	self assert: (aClass includesSelector: #fortyTwo).
	self applyInputEntry.
	self deny: (aClass includesSelector: #fortyTwo).
