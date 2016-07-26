instance creation
merging: listOfRects 
	"A number of callers of merge: should use this method."
	| minX minY maxX maxY |
	listOfRects do: 
		[ :r | 
		minX 
			ifNil: 
				[ minX := r topLeft x.
				minY := r topLeft y.
				maxX := r bottomRight x.
				maxY := r bottomRight y ]
			ifNotNil: 
				[ minX := minX min: r topLeft x.
				minY := minY min: r topLeft y.
				maxX := maxX max: r bottomRight x.
				maxY := maxY max: r bottomRight y ] ].
	^ minX @ minY corner: maxX @ maxY