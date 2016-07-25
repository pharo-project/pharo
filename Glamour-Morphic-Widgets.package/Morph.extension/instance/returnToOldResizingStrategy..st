*glamour-morphic-widgets
returnToOldResizingStrategy: aCollection

	|tmpSubmorphResizing|
	self submorphs with: aCollection third do: [:aMorph :resizingParameter |
		aMorph returnToOldResizingStrategy: resizingParameter.
		].
	self hResizing: aCollection first.
	self vResizing: aCollection second.