tests
testMethodAdditionWithMethodAlreadyRemoved

	| aClass anEntry |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	anEntry := monitor log head.

	aClass removeSelector: #fortyTwo.
	
	self assert: (anEntry content isKindOf: EpMethodAddition).
	self deny: (aClass includesSelector: #fortyTwo).
	self revert: { anEntry }.
	self deny: (aClass includesSelector: #fortyTwo).