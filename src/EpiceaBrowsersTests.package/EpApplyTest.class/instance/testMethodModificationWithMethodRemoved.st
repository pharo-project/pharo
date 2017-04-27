tests
testMethodModificationWithMethodRemoved

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass compile: 'fortyTwo ^nil' classified: 'number'.
	self setHeadAsInputEntry.

	aClass removeSelector: #fortyTwo.

	self assert: (inputEntry content isKindOf: EpMethodModification).
	self deny: (aClass includesSelector: #fortyTwo).
	self applyInputEntry.
	self assert: (aClass includesSelector: #fortyTwo).
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.
