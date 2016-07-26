organization updating
applyChangesOfNewTraitCompositionReplacing: oldComposition
	| changedSelectors oldMethodDict |
	oldMethodDict := self methodDict copy.
	changedSelectors := super applyChangesOfNewTraitCompositionReplacing: oldComposition.
	self noteRecategorizedSelectors: changedSelectors oldComposition: oldComposition.
	self noteChangesFrom: oldMethodDict.
	^ changedSelectors.