pixel accessing
isTransparentAt: aPoint 
	"Return true if the receiver is transparent at the given point."

	^ (self colorAt: aPoint) isTransparent
