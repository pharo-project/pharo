enumerating
withIndicesCollect: aBlock
	
	|i r|
	i := 0.
	r := contents shallowCopy.
	1 to: numberOfRows do: [ :row |
		1 to: numberOfColumns do: [ :column |
			i := i+1.
			r at: i put: (aBlock value: (r at: i) value: row value: column)]].
	^ self class rows: numberOfRows columns: numberOfColumns contents: r
