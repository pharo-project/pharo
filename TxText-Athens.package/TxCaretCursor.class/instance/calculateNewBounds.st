accessing
calculateNewBounds
	| topLeft bottomRight |

	"the top point in view is relative to view, 
	since morphic uses absolute coordinates we should add view position here"
	topLeft := owner layoutViewMorph topLeft + layoutPosition topPointInView.
	bottomRight := topLeft + (2@layoutPosition line height).
	
	"we round bounds here to avoid clipping artefacts"
	^ topLeft floor corner: bottomRight ceiling