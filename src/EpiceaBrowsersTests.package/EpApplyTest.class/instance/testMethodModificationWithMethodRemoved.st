tests
testMethodModificationWithMethodRemoved

	| aClass anEntry |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass compile: 'fortyTwo ^nil' classified: 'number'.
	anEntry := monitor log head.

	aClass removeSelector: #fortyTwo.

	self assert: (anEntry content isKindOf: EpMethodModification).
	self deny: (aClass includesSelector: #fortyTwo).
	self apply: { anEntry }.
	self assert: (aClass includesSelector: #fortyTwo).
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.
