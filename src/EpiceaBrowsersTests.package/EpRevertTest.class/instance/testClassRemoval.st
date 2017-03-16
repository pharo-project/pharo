tests
testClassRemoval

	| aClass className anEntry |
	aClass := classFactory newClass.
	className := aClass name.
	aClass removeFromSystem.
	anEntry := monitor log head.

	self assert: (anEntry content isKindOf: EpClassRemoval).
	self deny: (self class environment hasClassNamed: className).
	self revert: { anEntry }.
	self assert: (self class environment hasClassNamed: className).
