traits
applyChangesOfNewTraitCompositionReplacing: oldComposition
	| changedSelectors |
	changedSelectors := self traitComposition
		changedSelectorsComparedTo: oldComposition.
	changedSelectors isEmpty ifFalse: [
		self noteChangedSelectors: changedSelectors].
	self traitComposition isEmpty ifTrue: [
		self purgeLocalSelectors].
	^changedSelectors