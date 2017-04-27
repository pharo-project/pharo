tests
testTraitModificationOfTraitComposition
	<expectedFailure>
	"Failure: Currently, there is no system announcement when the trait composition is changed. 
	Epicea, confussingly, only registers method addition and modification events 
	when the trait is installed."
	
	| aTrait |
	aClass := classFactory newTrait.
	aTrait := classFactory newTrait.
	aClass setTraitComposition: aTrait asTraitComposition.

	self assert: (self countLogEventsWith: EpTraitModification) equals: 1.
	self assert: (self countLogEventsWith: EpMethodAddition) equals: 0.
	self assert: (self countLogEventsWith: EpMethodModification) equals: 0.
