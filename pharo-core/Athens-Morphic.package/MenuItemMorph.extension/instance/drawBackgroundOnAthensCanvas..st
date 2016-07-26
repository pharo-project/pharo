*Athens-Morphic
drawBackgroundOnAthensCanvas: anAthensCanvas
	(isSelected and: [ isEnabled ])
		ifFalse: [ ^ self ].
	anAthensCanvas setPaint: self selectionFillStyle.
	anAthensCanvas drawShape: (0 @ 0 extent: self extent)