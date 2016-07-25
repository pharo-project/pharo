private
computeShadow
	| canvas bounds theText |
	bounds := self bounds.
	theText := textMorph.
	canvas := (Display defaultCanvasClass extent: bounds extent depth: 1)
		asShadowDrawingCanvas: Color black.
	canvas translateBy: bounds topLeft negated during:[:tempCanvas| | back |
		self fillsOwner
			ifTrue: [tempCanvas fullDrawMorph: (theText owner copyWithoutSubmorph: theText)]
			ifFalse: [tempCanvas fillRectangle: textMorph bounds color: Color black].
		self avoidsOcclusions ifTrue:
			[back := tempCanvas form deepCopy.
			tempCanvas form fillWhite.
			theText owner submorphsInFrontOf: theText do:
				[:m | (textMorph isLinkedTo: m)
					ifTrue: []
					ifFalse: [tempCanvas fullDrawMorph: m]].
			back displayOn: tempCanvas form at: 0@0 rule: Form reverse].
	].
	shadowForm := canvas form offset: bounds topLeft.
	vertProfile := shadowForm  yTallyPixelValue: 1 orNot: false.
	rectangleCache := Dictionary new.
	^ shadowForm