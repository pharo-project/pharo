other
clippingTo: aRect do: aBlock 
	"Display clippingTo: Rectangle fromUser do:"
	| saveClip |
	saveClip := clippingBox.
	clippingBox := aRect.
	aBlock value.
	clippingBox := saveClip