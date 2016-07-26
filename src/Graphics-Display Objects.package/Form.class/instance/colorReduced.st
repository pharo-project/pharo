converting
colorReduced
	"Return a color-reduced ColorForm version of the receiver, if possible, or the receiver itself if not."

	| tally tallyDepth colorCount newForm cm oldPixelValues newFormColors nextColorIndex c |
	tally := self tallyPixelValues asArray.
	tallyDepth := (tally size log: 2) asInteger.
	colorCount := 0.
	tally do: [:n | n > 0 ifTrue: [colorCount := colorCount + 1]].
	(tally at: 1) = 0 ifTrue: [colorCount := colorCount + 1].  "include transparent"
	colorCount > 256 ifTrue: [^ self].  "cannot reduce"
	newForm := self formForColorCount: colorCount.

	"build an array of just the colors used, and a color map to translate
	 old pixel values to their indices into this color array"
	cm := Bitmap new: tally size.
	oldPixelValues := self colormapIfNeededForDepth: 32.
	newFormColors := Array new: colorCount.
	newFormColors at: 1 put: Color transparent.
	nextColorIndex := 2.
	2 to: cm size do: [:i |
		(tally at: i) > 0 ifTrue: [
			oldPixelValues 
				ifNil: [c := Color colorFromPixelValue: i - 1 depth: tallyDepth]
				ifNotNil: [c := Color colorFromPixelValue: (oldPixelValues at: i) depth: 32].
			newFormColors at: nextColorIndex put: c.
			cm at: i put: nextColorIndex - 1.  "pixel values are zero-based indices"
			nextColorIndex := nextColorIndex + 1]].

	"copy pixels into new ColorForm, mapping to new pixel values"
	newForm copyBits: self boundingBox
		from: self
		at: 0@0
		clippingBox: self boundingBox
		rule: Form over
		fillColor: nil
		map: cm.
	newForm colors: newFormColors.
	newForm offset: offset.
	^ newForm
