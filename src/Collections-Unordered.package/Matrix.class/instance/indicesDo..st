enumerating
indicesDo: aBlock
	1 to: numberOfRows do: [ :row |
		1 to: numberOfColumns do: [ :column |
			aBlock value: row value: column]].