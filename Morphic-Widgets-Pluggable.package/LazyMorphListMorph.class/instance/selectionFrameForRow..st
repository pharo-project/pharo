as yet unclassified
selectionFrameForRow: row
	"Answer the selection frame rectangle."

	|frame|
	frame := self drawBoundsForRow: row.
	frame := frame expandBy: (self cellInset // 2).
	self cellInset odd ifTrue: [frame := frame bottom: frame bottom + 1].
	frame := frame intersect: self bounds ifNone: [ self error: 'should not happen' ].
	frame := self bounds: frame in: listSource.
	frame := self
		bounds: ((frame left: listSource innerBounds left) right: listSource innerBounds right)
		from: listSource.
	^frame