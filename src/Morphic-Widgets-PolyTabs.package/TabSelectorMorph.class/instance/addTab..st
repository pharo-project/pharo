adding
addTab: aStringOrMorph
	"Add a new tab with the given text."
	
	self tabs add: (self newLabelMorph: aStringOrMorph).
	self updateTabs