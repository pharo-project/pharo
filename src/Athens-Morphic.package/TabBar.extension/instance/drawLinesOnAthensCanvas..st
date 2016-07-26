*Athens-Morphic
drawLinesOnAthensCanvas: anAthensCanvas
	| path |
	path := anAthensCanvas
		createPath: [ :builder | 
			builder moveTo: self bottomLeft + (0 @ -1).
			builder lineTo: self bottomRight + (0 @ -1) ].
	(anAthensCanvas setStrokePaint: self borderColor) width: 5.
	anAthensCanvas drawShape: path