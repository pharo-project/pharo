row/column operations
swapColumn: anIndex withColumn: anotherIndex
	|a b|

	a := self indexForRow: 1 andColumn: anIndex.
	b := self indexForRow: 1 andColumn: anotherIndex.
	numberOfRows timesRepeat: [
		contents swap: a with: b.
		a := a + numberOfColumns.
		b := b + numberOfColumns].
