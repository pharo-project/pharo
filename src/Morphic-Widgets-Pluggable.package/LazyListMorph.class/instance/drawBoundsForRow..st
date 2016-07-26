list management
drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!"

	| topLeft drawBounds item height |
	item := [ self getListItem: row ]
		on: SubscriptOutOfBounds
		do: [ :ex | self getListItem: (row min: self getListSize) ].
	height := item heightToDisplayInList: self.
	topLeft := self topLeft x @ (self topLeft y + ((row - 1) * height)).
	drawBounds := topLeft extent: self width @ height.
	^ drawBounds