geometry
extent: aPoint
	| size oldRotationCenter |
	oldRotationCenter := self rotationCenter.
	size := aPoint x min: aPoint y.
	super extent: size @ size.
	self rotationCenter: oldRotationCenter.