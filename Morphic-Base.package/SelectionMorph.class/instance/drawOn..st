drawing
drawOn: aCanvas

	| canvas form1 form2 box |
	super drawOn: aCanvas.
	box := self bounds.
	selectedItems do: [:m | box := box merge: m fullBounds].
	box := box expandBy: 1.
	canvas := Display defaultCanvasClass extent: box extent depth: 8.
	canvas translateBy: box topLeft negated
		during: [:tempCanvas | selectedItems do: [:m | tempCanvas fullDrawMorph: m]].
	form1 := (Form extent: box extent) copyBits: (0@0 extent: box extent) from: canvas form at: 0@0 colorMap: (Color maskingMap: 8).
	form2 := Form extent: box extent.
	(0@0) fourNeighbors do: [:d | form1 displayOn: form2 at: d rule: Form under].
	form1 displayOn: form2 at: 0@0 rule: Form erase.
	aCanvas stencil: form2
		at: box topLeft
		sourceRect: form2 boundingBox
		color: self borderColor
