adding/removing
addTab: aStringOrMorph
	"Add a new tab with the given text."
	
	self tabs add: (self newLabelBrick: aStringOrMorph).
	self updateTabs