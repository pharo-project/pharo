*Glamour-Morphic-Brick
brickImageForm: aColor
	| canvas rect |
	rect := self fullBounds origin extent: self measureContents.
	canvas := Display defaultCanvasClass extent: rect extent depth: Display depth.
	canvas form fillColor: aColor. 
	canvas translateBy: rect topLeft negated
		during: [ :tempCanvas | tempCanvas fullDrawMorph: self ].
	^ canvas form offset: rect topLeft