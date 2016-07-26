drawing
drawOn: aCanvas
	"Draw the indicators for the mappings."
	
	super drawOn: aCanvas.
	aCanvas translateBy: self topLeft clippingTo: self clippingBounds during: [:c |
		self mappings do: [:j | j drawOn: c]]