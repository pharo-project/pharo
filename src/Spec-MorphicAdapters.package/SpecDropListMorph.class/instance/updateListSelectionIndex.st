private
updateListSelectionIndex

	currentIcon ifNotNil: [ self removeMorph: currentIcon ].
		
	super updateListSelectionIndex.
	
	self selectedItem ifNotNil: [ :item | 
		(self getIconFor: item) ifNotNil: [:icon | 
			currentIcon := icon asMorph.
			self addMorphFront: currentIcon ] ] 