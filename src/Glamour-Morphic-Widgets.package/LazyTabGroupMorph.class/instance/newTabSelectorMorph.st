building
newTabSelectorMorph
	"Answer a new tab selector morph"

	^ GLMTabSelectorBrick new
		minHeight: self theme buttonMinHeight;
		hSpaceFill;
		vResizing: #shrinkWrap