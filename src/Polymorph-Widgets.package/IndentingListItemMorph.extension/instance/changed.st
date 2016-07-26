*Polymorph-Widgets
changed
	"Need to invalidate the selection frame."

	container ifNil: [^super changed].
	self invalidRect: self selectionFrame.
	super changed