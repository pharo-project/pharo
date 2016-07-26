drawing
drawOnAthensCanvas: anAthensCanvas
	| visibleRectangle |
	super drawOnAthensCanvas: anAthensCanvas.
	self paragraph selectionStart ifNil: [ ^ self ].
	self paragraph selectionStop ifNil: [ ^ self ].
	self paragraph lines ifNil: [ ^ self ].
	anAthensCanvas
		clipBy: self bounds
		during: [ 
			| tl br |
			anAthensCanvas setShape: self verticalSeparatorBounds.
			anAthensCanvas setPaint: self verticalSeparatorColor.
			anAthensCanvas draw.
			visibleRectangle := anAthensCanvas clipRect.
			tl := self lineIndexForPoint: visibleRectangle topLeft.
			br := self lineIndexForPoint: visibleRectangle bottomRight.
			tl to: br do: [ :i | 
				((self paragraphProvider segmentsAtLine: i) select: [ :s | s icon notNil ])
					ifNotEmpty: [ :segments | 
						| s pos |
						s := segments first.
						pos := self iconPositionOfSegment: s.
						s displayIconAt: pos onAthensCanvas: anAthensCanvas.
						segments size > 1
							ifTrue: [ 
								| r ygap path |
								ygap := s icon height + 1.
								r := pos + (0 @ ygap) corner: (pos x + 12) @ (pos y + ygap + 2).
								anAthensCanvas setShape: (r expandBy: 2).
								anAthensCanvas setPaint: (Color white alpha: 0.4).
								anAthensCanvas draw.
								path := anAthensCanvas
									createPath: [ :builder | 
										builder absolute.
										builder moveTo: r topLeft + (2@0).
										builder lineTo: r topRight + (2@0)].
								(anAthensCanvas setStrokePaint: Color black)
									width: 1;
									dashes: {2.4} offset: 0.
								anAthensCanvas drawShape: path ] ] ] ]