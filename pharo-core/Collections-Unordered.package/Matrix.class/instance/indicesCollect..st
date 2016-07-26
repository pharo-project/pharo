enumerating
indicesCollect: aBlock
	
	| r i |
	r := Array new: numberOfRows * numberOfColumns.
	i := 0.
	1 to: numberOfRows do: [:row |
		1 to: numberOfColumns do: [:column |
			r at: (i := i+1) put: (aBlock value: row value: column)]].
	^ self class rows: numberOfRows columns: numberOfColumns contents: r