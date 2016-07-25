drawing
drawOn: aCanvas
	| visibleRectangle |
	super drawOn: aCanvas.
	self paragraph selectionStart ifNil: [ ^ self ].
	self paragraph selectionStop ifNil: [ ^ self ].
	self paragraph lines ifNil: [ ^ self ].
	aCanvas
		clipBy: self bounds
		during: [ :clippedCanvas | 
			| tl br |
			clippedCanvas fillRectangle: self verticalSeparatorBounds color: self verticalSeparatorColor.
			visibleRectangle := clippedCanvas clipRect.
			tl := self lineIndexForPoint: visibleRectangle topLeft.
			br := self lineIndexForPoint: visibleRectangle bottomRight.
			tl to: br do: [ :i | 
				((self paragraphProvider segmentsAtLine: i) select: [ :s | s icon notNil ])
					ifNotEmpty: [ :segments | 
						| s pos |
						s := segments first.
						s displayIconAt: (pos := self iconPositionOfSegment: s) on: aCanvas.
						segments size > 1
							ifTrue: [ 
								| r ygap |
								ygap := s icon height + 1.
								r := pos + (0 @ ygap) corner: (pos x + 12) @ (pos y + ygap + 2).
								aCanvas fillRectangle: (r expandBy: 2) color: (Color white alpha: 0.4).
								aCanvas
									frameRectangle: r
									width: 2
									colors: {(Color white). (Color black)}
									dashes: {1. 1} ] ] ] ]