tests
testClassRemovalWithClassAdded

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	aClass removeFromSystem.
	self setHeadAsInputEntry.

	aClass := classFactory newClass.
	aClass rename: className.

	self assert: (inputEntry content isKindOf: EpClassRemoval).
	self assert: (self class environment hasClassNamed: className).
	self applyInputEntry.
	self deny: (self class environment hasClassNamed: className).
