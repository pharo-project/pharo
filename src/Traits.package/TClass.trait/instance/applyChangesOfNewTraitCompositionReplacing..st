traits
applyChangesOfNewTraitCompositionReplacing: oldComposition

	| changedSelectors |
	changedSelectors := super applyChangesOfNewTraitCompositionReplacing: oldComposition.
	self classSide noteNewBaseTraitCompositionApplied: self traitComposition.
	^ changedSelectors