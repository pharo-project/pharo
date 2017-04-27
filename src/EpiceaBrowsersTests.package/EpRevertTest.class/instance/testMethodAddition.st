tests
testMethodAddition

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	self setHeadAsInputEntry.

	self assert: (inputEntry content isKindOf: EpMethodAddition).
	self assert: (aClass includesSelector: #fortyTwo).
	self revertInputEntry.
	self deny: (aClass includesSelector: #fortyTwo).