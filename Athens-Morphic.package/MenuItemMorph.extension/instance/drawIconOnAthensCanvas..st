*Athens-Morphic
drawIconOnAthensCanvas: anAthensCanvas
	self hasIcon
		ifFalse: [ ^ self ].
	anAthensCanvas setPaint: self iconForm.
	anAthensCanvas drawShape: (0 @ 0 extent: self iconForm extent).
	anAthensCanvas pathTransform translateX: self iconForm width + 2 Y: 0