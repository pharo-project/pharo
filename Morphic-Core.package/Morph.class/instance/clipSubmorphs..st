drawing
clipSubmorphs: aBool
	"Drawing specific. If this property is set, clip the receiver's submorphs to the receiver's clipping bounds."
	self invalidRect: self fullBounds.
	
	aBool == self clipSubmorphs ifFalse:[
		self assureExtension.
		extension clipSubmorphs: aBool.
		self invalidRect: self fullBounds]