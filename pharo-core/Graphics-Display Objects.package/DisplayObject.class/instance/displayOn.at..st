displaying-generic
displayOn: aDisplayMedium at: aDisplayPoint 
	"Display the receiver located at aDisplayPoint with default settings for 
	rule and halftone."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: aDisplayMedium boundingBox
		rule: Form over
		fillColor: nil