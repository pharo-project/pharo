initialize
initializeWithBaseTrait: aTrait

	<localClassTraitMethod> 
    "This method is supposed to be local in ClassTrait because of a good reason.
    We use this pragma to test if ClassTrait does not contain some accidental 
    local selectors."

	self baseTrait: aTrait.
	self noteNewBaseTraitCompositionApplied: aTrait traitComposition.
	aTrait traitUsers do: [:each | self addUser: each classSide].
	