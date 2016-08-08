row/column operations
atRow: row put: aCollection
	|p|

	aCollection size = numberOfColumns ifFalse: [self error: 'wrong row size'].
	p := (self indexForRow: row andColumn: 1)-1.
	aCollection do: [:each | contents at: (p := p+1) put: each].
	^aCollection