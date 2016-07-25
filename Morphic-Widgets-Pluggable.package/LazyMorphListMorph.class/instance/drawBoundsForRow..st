as yet unclassified
drawBoundsForRow: row
	"Calculate the bounds that row should be drawn at.  This might be outside our bounds!"
	
	(row between: 1 and: listItems size)
		ifFalse: [^0@0 corner: 0@0].
	^(listItems at: row) bounds