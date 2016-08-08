copying
, aGrid
	"Answer a new grid having the same number of rows as the receiver and aGrid,
	 its columns being the columns of the receiver followed by the columns of aGrid."

	| newCont newCols anArray oldCols a b c |
	[ numberOfRows = aGrid numberOfRows ] assert.
	newCont := Array new: self size + aGrid size.
	anArray := aGrid privateContents.
	oldCols := aGrid numberOfColumns.
	newCols := numberOfColumns + oldCols.
	a := b := c := 1.
	1 to: numberOfRows do:
		[ :r | 
		newCont
			replaceFrom: a
			to: a + numberOfColumns - 1
			with: contents
			startingAt: b.
		newCont
			replaceFrom: a + numberOfColumns
			to: a + newCols - 1
			with: anArray
			startingAt: c.
		a := a + newCols.
		b := b + numberOfColumns.
		c := c + oldCols ].
	^ self class rows: numberOfRows columns: newCols contents: newCont