enumerating
withIndicesDo: aBlock
	
	| i |
	i := 0.
	1 to: numberOfRows do: [:row |
		1 to: numberOfColumns do: [:column |
			aBlock value: (contents at: (i := i+1)) value: row value: column]].
