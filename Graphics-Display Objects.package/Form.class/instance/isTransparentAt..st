pixel access
isTransparentAt: aPoint 
	"Return true if the receiver is transparent at the given point."

	self depth = 1 ifTrue: [^ false].  "no transparency at depth 1"
	^ (self pixelValueAt: aPoint) = (self pixelValueFor: Color transparent)
