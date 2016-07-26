color mapping
newColorMap 
	"Return an uninitialized color map array appropriate to this Form's depth."

	^ Bitmap new: (1 bitShift: (self depth min: 15))
