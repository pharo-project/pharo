as yet unclassified
drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!"
	| topLeft drawBounds item width height |
	
	item := self getListItem: row.
	height := (item collect: [:e | e heightToDisplayInList: self ]) max.
	width := self width.
		
	topLeft := self topLeft x @ (self topLeft y + ((row - 1) * (height))).
	drawBounds := topLeft extent: (width @ height).
	^drawBounds