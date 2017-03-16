tests
testMethodAdditionWithMethodRemoved

	| aClass anEntry |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	anEntry := monitor log head.

	aClass removeSelector: #fortyTwo.
	
	self assert: (anEntry content isKindOf: EpMethodAddition).
	self deny: (aClass includesSelector: #fortyTwo).
	self apply: { anEntry }.
	self assert: (aClass includesSelector: #fortyTwo).
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.
