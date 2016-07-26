row/column operations
atColumn: column put: aCollection
	| p |
	aCollection size = numberOfRows ifFalse: [ self error: 'wrong column size' ].
	p := (self indexForRow: 1 andColumn: column) - numberOfColumns.
	aCollection do: [ :each | contents at: (p := p + numberOfColumns) put: each ].
	^ aCollection
