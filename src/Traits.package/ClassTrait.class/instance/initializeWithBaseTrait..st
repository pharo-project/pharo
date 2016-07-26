initialize
initializeWithBaseTrait: aTrait
	self baseTrait: aTrait.
	self noteNewBaseTraitCompositionApplied: aTrait traitComposition.
	aTrait traitUsers do: [:each | self addUser: each classSide].
	