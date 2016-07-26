copying
, aMatrix
	"Answer a new matrix having the same number of rows as the receiver and aMatrix,
	 its columns being the columns of the receiver followed by the columns of aMatrix."
	|newCont newCols anArray oldCols a b c|

	[numberOfRows = aMatrix numberOfRows] assert.
	newCont := Array new: self size + aMatrix size.
	anArray := aMatrix privateContents.
	oldCols := aMatrix numberOfColumns.
	newCols := numberOfColumns + oldCols.
	a := b := c := 1.
	1 to: numberOfRows do: [:r |
		newCont replaceFrom: a to: a + numberOfColumns - 1 with: contents startingAt: b.
		newCont replaceFrom: a+numberOfColumns to: a + newCols - 1 with: anArray startingAt: c.
		a := a + newCols.
		b := b + numberOfColumns.
		c := c + oldCols].
	^self class rows: numberOfRows columns: newCols contents: newCont
		
