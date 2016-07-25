drawing
position: aPoint menuWidth: anInteger 
	| y x |
	arrowPosition := aPoint.
	y := aPoint y + self class height.
	y := y > Display height
				ifTrue: [Display height - self class height]
				ifFalse: [aPoint y - self itemHeight].
	x := aPoint x.
	x := x + self class width > Display width
				ifTrue: [
					arrowPosition := (self positionOnLeft: anInteger) @ aPoint y.
					arrowPosition x]
				ifFalse: [x].
	self position: x @ y