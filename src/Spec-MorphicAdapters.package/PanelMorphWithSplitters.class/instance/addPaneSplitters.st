private
addPaneSplitters
	"Add the vertical and horizontal pane splitters."

	self removePaneSplitters.
	self addPaneVSplitters.
	self addPaneHSplitters.
	self linkSplittersToSplitters