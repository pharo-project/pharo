private
noteRemovalOfAll: aCollection

	scroller removeAllMorphsIn: aCollection.
	(aCollection includes: selectedMorph) ifTrue: [self setSelectedMorph: nil].