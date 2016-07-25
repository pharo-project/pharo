enumerating
indicesInject: start into: aBlock

	|current|
	current := start.
	1 to: numberOfRows do: [ :row |
		1 to: numberOfColumns do: [ :column |
			current := aBlock value: current value: row value: column ] ].
	^ current