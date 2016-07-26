displaying-generic
displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle 
	"Display the receiver located at aDisplayPoint with default settings for 
	rule and halftone. Information to be displayed must be confined to the 
	area that intersects with clipRectangle."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: clipRectangle
		rule: Form over
		fillColor: nil