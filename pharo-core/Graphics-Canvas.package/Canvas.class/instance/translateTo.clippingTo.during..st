drawing-support
translateTo: newOrigin clippingTo: aRectangle during: aBlock
	"Set a new origin and clipping rectangle only during the execution of aBlock."
	self translateBy: newOrigin - self origin 
		clippingTo: (aRectangle translateBy: self origin negated) 
		during: aBlock