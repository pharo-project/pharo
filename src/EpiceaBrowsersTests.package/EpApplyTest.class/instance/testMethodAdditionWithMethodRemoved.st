tests
testMethodAdditionWithMethodRemoved

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.

	aClass removeSelector: #fortyTwo.
	
	self assert: (inputEntry content isKindOf: EpMethodAddition).
	self deny: (aClass includesSelector: #fortyTwo).
	self applyInputEntry.
	self assert: (aClass includesSelector: #fortyTwo).
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.
