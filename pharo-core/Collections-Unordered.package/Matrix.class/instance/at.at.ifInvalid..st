accessing
at: rowNumber at: columnNumber ifInvalid: value
	"If rowNumber,columnNumber is a valid index for the receiver, answer the corresponding element. Otherwise, answer value."

	(rowNumber between: 1 and: numberOfRows) ifFalse: [ ^ value ].
	(columnNumber between: 1 and: numberOfColumns) ifFalse: [ ^ value ].
	^contents at: (rowNumber - 1) * numberOfColumns + columnNumber
