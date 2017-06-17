tests
testMetaclassInstanceVariableAdditionWithTrait

	| aTrait |
	aClass := classFactory newClass.
	aTrait := classFactory newTrait.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
	aClass theMetaClass uses: aTrait asTraitComposition instanceVariableNames: 'x'.	
	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
