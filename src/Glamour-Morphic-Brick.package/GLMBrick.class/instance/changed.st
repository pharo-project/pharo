morph-drawing
changed
	"Report that the area occupied by this morph should be redrawn.
	Fixed to include submorphs outside the outerBounds."
	
	"fixed direct reference to fullBounds, causing wrong results, because fullBounds are not being updated when parent's or child's bounds changed"
	self world
		ifNil: [ ^ self ].
	
	self invalidRect: self globalClippingBounds