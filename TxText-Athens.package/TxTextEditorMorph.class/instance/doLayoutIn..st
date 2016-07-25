updating
doLayoutIn: layoutBounds

	"force view morph to always cover layout bounds"
	layoutViewMorph bounds: layoutBounds.
	self updateCursor.
	super doLayoutIn:layoutBounds.
	