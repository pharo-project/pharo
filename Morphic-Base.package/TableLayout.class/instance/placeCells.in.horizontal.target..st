layout
placeCells: arrangement in: newBounds horizontal: aBool target: aMorph 
	"Place the morphs within the cells accordingly"

	| xDir yDir anchor yDist place cell xDist cellRect corner inset |
	inset := properties cellInset.
	(inset isNumber and: [inset isZero]) ifTrue: [inset := nil].
	aBool 
		ifTrue: 
			["horizontal layout"

			properties listDirection == #rightToLeft 
				ifTrue: 
					[xDir := -1 @ 0.
					properties wrapDirection == #bottomToTop 
						ifTrue: 
							[yDir := 0 @ -1.
							anchor := newBounds bottomRight]
						ifFalse: 
							[yDir := 0 @ 1.
							anchor := newBounds topRight]]
				ifFalse: 
					[xDir := 1 @ 0.
					properties wrapDirection == #bottomToTop 
						ifTrue: 
							[yDir := 0 @ -1.
							anchor := newBounds bottomLeft]
						ifFalse: 
							[yDir := 0 @ 1.
							anchor := newBounds topLeft]]]
		ifFalse: 
			["vertical layout"

			properties listDirection == #bottomToTop 
				ifTrue: 
					[xDir := 0 @ -1.
					properties wrapDirection == #rightToLeft 
						ifTrue: 
							[yDir := -1 @ 0.
							anchor := newBounds bottomRight]
						ifFalse: 
							[yDir := 1 @ 0.
							anchor := newBounds bottomLeft]]
				ifFalse: 
					[xDir := 0 @ 1.
					anchor := properties wrapDirection == #rightToLeft 
								ifTrue: 
									[yDir := -1 @ 0.
									newBounds topRight]
								ifFalse: 
									[yDir := 1 @ 0.
									newBounds topLeft]]].
	1 to: arrangement size
		do: 
			[:i | 
			cell := arrangement at: i.
			cell extraSpace ifNotNil: [anchor := anchor + (cell extraSpace y * yDir)].
			yDist := cell cellSize y * yDir.	"secondary advance direction"
			place := anchor.
			cell := cell nextCell.
			[cell isNil] whileFalse: 
					[cell extraSpace ifNotNil: [place := place + (cell extraSpace x * xDir)].
					xDist := cell cellSize x * xDir.	"primary advance direction"
					corner := place + xDist + yDist.
					cellRect := Rectangle origin: (place min: corner)
								corner: (place max: corner).
					inset ifNotNil: [cellRect := cellRect insetBy: inset].
					cell target layoutInBounds: cellRect.
					place := place + xDist.
					cell := cell nextCell].
			anchor := anchor + yDist]