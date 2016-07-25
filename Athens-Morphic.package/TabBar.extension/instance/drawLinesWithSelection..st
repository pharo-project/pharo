*Athens-Morphic
drawLinesWithSelection: anAthensCanvas
	| path |
	path := anAthensCanvas
		createPath: [ :builder | 
			builder absolute.
			builder moveTo: self bottomLeft + (0 @ -1).
			builder lineTo: self selectedTab bottomLeft.
			builder moveTo:  self selectedTab  bottomRight.
			builder lineTo: self bottomRight + (0 @ -1)].
	(anAthensCanvas setStrokePaint: self borderColor) width: 1.
	anAthensCanvas drawShape: path.

	path := anAthensCanvas
		createPath: [ :builder | 
			builder absolute.			
			builder moveTo: self selectedTab bottomLeft.
			builder lineTo: self selectedTab  bottomRight].
	(anAthensCanvas setStrokePaint: self selectedColor) width:1.
	anAthensCanvas drawShape: path.
	