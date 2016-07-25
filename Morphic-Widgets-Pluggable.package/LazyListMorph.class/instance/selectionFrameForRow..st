row management
selectionFrameForRow: row
	"Answer the selection frame rectangle."

	|frame| 
	frame := self drawBoundsForRow: row.
	frame := frame intersect: self bounds .
	frame := self bounds: frame in: listSource.
	frame := self
		bounds: ((frame left: listSource innerBounds left) right: listSource innerBounds right)
		from: listSource.
	^frame