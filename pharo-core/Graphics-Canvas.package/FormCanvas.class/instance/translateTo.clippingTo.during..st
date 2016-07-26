drawing-support
translateTo: newOrigin clippingTo: aRectangle during: aBlock
	"Set a new origin and clipping rectangle only during the execution of aBlock."
	aBlock value: (self copyOrigin: newOrigin clipRect: aRectangle)