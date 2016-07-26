initialization
initialize
	"initialize the receiver"
	super initialize.
	""
	selectedItem := nil.
	activeSubMenu := nil.
	fillsOwner := true.
	avoidVisibleBordersAtEdge := true.
	autoGradient := self theme preferGradientFill.
	""
	self setDefaultParameters.
	""
	self beFloating.
	""
	self layoutInset: 0.
	