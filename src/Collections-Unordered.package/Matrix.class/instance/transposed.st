row/column operations
transposed
	[numberOfRows = numberOfColumns] assert.
	^ self indicesCollect: [ :row :column | self at: column at: row ]