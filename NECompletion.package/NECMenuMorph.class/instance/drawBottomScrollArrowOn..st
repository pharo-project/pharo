drawing
drawBottomScrollArrowOn: aCanvas 
	| aPoligon point arrowHeight |
	point := self bounds bottomLeft translateBy: 6 @ -12.
	arrowHeight := self class scrollArrowSize.
	aPoligon := Array 
		with: point
		with: (point translateBy: arrowHeight @ 0)
		with: (point translateBy: (arrowHeight / 2) @ arrowHeight).
	aCanvas 
		drawPolygon: aPoligon
		fillStyle: Color black