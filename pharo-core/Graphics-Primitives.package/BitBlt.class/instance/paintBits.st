private
paintBits
	"Perform the paint operation, which requires two calls to BitBlt."
	| color oldMap saveRule |
	sourceForm depth = 1 ifFalse: 
		[ ^ self error: 'paint operation is only defined for 1-bit deep sourceForms' ].
	saveRule := combinationRule.
	color := halftoneForm.
	halftoneForm := nil.
	oldMap := colorMap.
	"Map 1's to ALL ones, not just one"
	self colorMap: (Bitmap 
			with: 0
			with: 4294967295).
	combinationRule := Form erase.
	self copyBits.	"Erase the dest wherever the source is 1"
	halftoneForm := color.
	combinationRule := Form under.
	self copyBits.	"then OR, with whatever color, into the hole"
	colorMap := oldMap.
	combinationRule := saveRule

	" | dot |
dot := Form dotOfSize: 32.
((BitBlt destForm: Display
		sourceForm: dot
		fillColor: Color lightGray
		combinationRule: Form paint
		destOrigin: Sensor cursorPoint
		sourceOrigin: 0@0
		extent: dot extent
		clipRect: Display boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF)) copyBits"