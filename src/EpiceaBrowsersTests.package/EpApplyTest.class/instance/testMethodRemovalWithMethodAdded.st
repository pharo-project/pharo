tests
testMethodRemovalWithMethodAdded

	| aClass anEntry |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	anEntry := monitor log head.

	aClass compile: 'fortyTwo ^42' classified: 'number'.

	self assert: (anEntry content isKindOf: EpMethodRemoval).
	self assert: (aClass includesSelector: #fortyTwo).
	self apply: { anEntry }.
	self deny: (aClass includesSelector: #fortyTwo).
