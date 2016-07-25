displaying-generic
displayAt: aDisplayPoint 
	"Display the receiver located at aDisplayPoint with default settings for 
	the displayMedium, rule and halftone."

	self displayOn: Display
		at: aDisplayPoint
		clippingBox: Display boundingBox
		rule: Form over
		fillColor: nil