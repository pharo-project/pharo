drawing
drawArrowOn: aCanvas
	| point factor poligon |
	factor := self itemHeight.
	point := arrowPosition.
	poligon := OrderedCollection new.
	poligon add: point.
	poligon add: (point := point translateBy: (factor / 2) @ 0).
	poligon add: (point := point translateBy: 0 @ (factor * -0.5)).
	poligon add: (point := point translateBy: factor @ factor).
	poligon add: (point := point translateBy: (factor * -1) @ factor).
	poligon add: (point := point translateBy: 0 @ (factor * -0.5)).
	poligon add: (point := point translateBy: (factor * -0.5) @ 0).
	aCanvas drawPolygon: poligon fillStyle: self scrollColor