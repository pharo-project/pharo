selection
setSelectedMorph: aMorph
	self changeModelSelection: (scroller submorphs indexOf: aMorph)