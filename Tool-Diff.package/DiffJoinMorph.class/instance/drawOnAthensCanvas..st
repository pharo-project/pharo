drawing
drawOnAthensCanvas: anAthensCanvas
	"Draw the indicators for the mappings."

	super drawOnAthensCanvas: anAthensCanvas. 
	(anAthensCanvas asCanvasWrapper ) translateBy: self topLeft clippingTo: self clippingBounds during: [:c |
		self mappings do: [:j | j drawOn: c]]