accessing
nextPutImage: aForm

	| reduced tempForm |
	aForm unhibernate.
	aForm depth > 8 ifTrue:[
		reduced := aForm colorReduced.  "minimize depth"
		reduced depth > 8 ifTrue: [
			"Not enough color space; do it the hard way."
			reduced := reduced asFormOfDepth: 8].
	] ifFalse:[reduced := aForm].
	reduced depth < 8 ifTrue: [
		"writeBitData: expects depth of 8"
		tempForm := reduced class extent: reduced extent depth: 8.
		(reduced isColorForm) ifTrue:[
			tempForm
				copyBits: reduced boundingBox
				from: reduced at: 0@0
				clippingBox: reduced boundingBox
				rule: Form over
				fillColor: nil
				map: nil.
			tempForm colors: reduced colors.
		] ifFalse: [reduced displayOn: tempForm].
		reduced := tempForm.
	].
	(reduced isColorForm) ifTrue:[
		(reduced colorsUsed includes: Color transparent) ifTrue: [
			transparentIndex := (reduced colors indexOf: Color transparent) - 1.
		]
	] ifFalse: [transparentIndex := nil].
	width := reduced width.
	height := reduced height.
	bitsPerPixel := reduced depth.
	colorPalette := reduced colormapIfNeededForDepth: 32.
	interlace := false.
	self writeHeader.
	self writeBitData: reduced bits.
