adding
addTab: aStringOrMorph selected: selectedStringOrMorph
	"Add a new tab with the given text or morph and alternate for when selected."
	
	self tabs add: (self newLabelMorph: aStringOrMorph selected: selectedStringOrMorph).
	self updateTabs