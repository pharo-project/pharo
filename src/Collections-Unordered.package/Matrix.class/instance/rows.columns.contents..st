private
rows: rows columns: columns contents: anArray
	[rows isInteger and: [rows >= 0]] assert.
	[columns isInteger and: [columns >= 0]] assert.
	[rows * columns = anArray size] assert.
	numberOfRows := rows.
	numberOfColumns := columns.
	contents := anArray.
	^self