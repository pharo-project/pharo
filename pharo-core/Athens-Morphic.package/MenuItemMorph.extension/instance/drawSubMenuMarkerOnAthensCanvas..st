*Athens-Morphic
drawSubMenuMarkerOnAthensCanvas: anAthensCanvas
	self hasSubMenu
		ifFalse: [ ^ self ].
	anAthensCanvas pathTransform
		restoreAfter: [ 
			anAthensCanvas pathTransform translateX: self extent x - self subMenuMarker width Y: 0.
			anAthensCanvas pathTransform translateX: 0 Y: (self subMenuMarker height //2).
			anAthensCanvas setPaint: self subMenuMarker.
			anAthensCanvas drawShape: (0 @ 0 extent: self subMenuMarker extent) ]