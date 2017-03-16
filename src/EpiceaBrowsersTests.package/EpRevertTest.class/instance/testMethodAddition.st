tests
testMethodAddition

	| aClass anEntry |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	anEntry := monitor log head.

	self assert: (anEntry content isKindOf: EpMethodAddition).
	self assert: (aClass includesSelector: #fortyTwo).
	self revert: { anEntry }.
	self deny: (aClass includesSelector: #fortyTwo).