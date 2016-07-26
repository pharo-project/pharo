private
insertNewMorphs: morphList

	scroller addAllMorphs: morphList.
	self adjustSubmorphPositions.
	self selection: self getCurrentSelectionItem.
	self setScrollDeltas.
