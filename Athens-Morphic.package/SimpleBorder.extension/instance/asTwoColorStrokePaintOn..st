*Athens-Morphic
asTwoColorStrokePaintOn: anAthensCanvas
	^ AthensBorderStrokePaint
		topLeftColor: self topLeftColor
		bottomRightColor: self bottomRightColor
		width: self width
		on: anAthensCanvas