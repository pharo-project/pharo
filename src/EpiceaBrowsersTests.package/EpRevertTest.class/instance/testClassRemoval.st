tests
testClassRemoval

	| aClass className category |
	aClass := classFactory newClass.
	aClass theMetaClass instanceVariableNames: 'hey'.
	className := aClass name.
	category := aClass category.
	aClass removeFromSystem.
	self setHeadAsInputEntry.

	self assert: (inputEntry content isKindOf: EpClassRemoval).
	self deny: (self class environment hasClassNamed: className).
	self revertInputEntry.
	self assert: (self class environment hasClassNamed: className).
	self assert: (self class environment classNamed: className) category equals: category.
	self assert: (self class environment classNamed: className) theMetaClass instVarNames equals: #('hey').