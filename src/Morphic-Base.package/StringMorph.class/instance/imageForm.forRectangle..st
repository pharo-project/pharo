drawing
imageForm: depth forRectangle: rect
	| canvas |
	canvas := Display defaultCanvasClass extent: rect extent depth: depth.
	
	backgroundColor isTransparent ifTrue: [ 
		canvas form fillColor: self theme backgroundColor.
	].
	canvas translateBy: rect topLeft negated
		during:[:tempCanvas| tempCanvas fullDrawMorph: self].
	^ canvas form offset: rect topLeft