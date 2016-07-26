copying
,, aMatrix
	"Answer a new matrix having the same number of columns as the receiver and aMatrix,
	 its rows being the rows of the receiver followed by the rows of aMatrix."

	[numberOfColumns = aMatrix numberOfColumns] assert.
	^self class 
			rows: numberOfRows + aMatrix numberOfRows 
			columns: numberOfColumns
			contents: contents , aMatrix privateContents
