drawing-images
paintImage: aForm at: aPoint
	"Draw the given Form, which is assumed to be a Form or ColorForm following the convention that zero is the transparent pixel value."

	self paintImage: aForm
		at: aPoint
		sourceRect: aForm boundingBox
