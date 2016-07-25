drawing
imageForm: depth backgroundColor: aColor forRectangle: rect
	| canvas |
	canvas := Display defaultCanvasClass extent: rect extent depth: depth.
	canvas translateBy: rect topLeft negated
		during:[:tempCanvas| 
			tempCanvas fillRectangle: rect color: aColor.
			tempCanvas fullDrawMorph: self].
	^ canvas form offset: rect topLeft