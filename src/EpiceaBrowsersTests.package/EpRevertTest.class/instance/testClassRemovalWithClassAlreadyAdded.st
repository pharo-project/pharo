tests
testClassRemovalWithClassAlreadyAdded

	| aClass className anEntry |
	aClass := classFactory newClass.
	className := aClass name.
	aClass removeFromSystem.
	anEntry := monitor log head.

	aClass := classFactory newClass.
	aClass rename: className.

	self assert: (anEntry content isKindOf: EpClassRemoval).
	self assert: (self class environment hasClassNamed: className).
	self revert: { anEntry }.
	self assert: (self class environment hasClassNamed: className).
