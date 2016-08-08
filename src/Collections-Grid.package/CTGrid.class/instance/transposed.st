row/column operations
transposed
	[numberOfRows = numberOfColumns] assert.
	^ self indicesCollect: [ :row :column | self atRow: column atColumn: row ]