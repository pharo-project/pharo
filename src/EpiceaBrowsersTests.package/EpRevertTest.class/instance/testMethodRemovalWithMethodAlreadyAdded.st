tests
testMethodRemovalWithMethodAlreadyAdded

	| aClass anEntry |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	anEntry := monitor log head.

	aClass compile: 'fortyTwo ^nil' classified: 'none'.

	self assert: (anEntry content isKindOf: EpMethodRemoval).
	self assert: (aClass includesSelector: #fortyTwo).
	self revert: { anEntry }.
	self assert: (aClass includesSelector: #fortyTwo).
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'.
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.
