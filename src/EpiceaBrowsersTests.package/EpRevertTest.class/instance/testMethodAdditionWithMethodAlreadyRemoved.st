tests
testMethodAdditionWithMethodAlreadyRemoved

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	self setHeadAsInputEntry.

	aClass removeSelector: #fortyTwo.
	
	self assert: (inputEntry content isKindOf: EpMethodAddition).
	self deny: (aClass includesSelector: #fortyTwo).
	self revertInputEntry.
	self deny: (aClass includesSelector: #fortyTwo).