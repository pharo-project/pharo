selection change
noteRemovalOfAll: aMorphList
	"When noting the removal of items, do not change their model status: the model will be in charge of it (i.e. either updates from the model or expand/collapse things)."

	aMorphList
		do: [ :each | 
			self selectedMorphList remove: each ifAbsent: [  ].
			lastClickedMorph == each
				ifTrue: [ lastClickedMorph := nil ] ]