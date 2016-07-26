private - layout
updateBounds
	"private - update the receiver's bounds"
	self updateExtent.
	self isFloating
		ifFalse: [self updatePosition]