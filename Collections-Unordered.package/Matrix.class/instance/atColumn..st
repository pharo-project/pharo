row/column operations
atColumn: column

	|p|
	p := (self indexForRow: 1 andColumn: column)-numberOfColumns.
	^(1 to: numberOfRows) collect: [:row | contents at: (p := p+numberOfColumns)]
