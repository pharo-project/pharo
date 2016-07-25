analyzing
xTallyPixelValue: pv orNot: not
	"Return an array of the number of pixels with value pv by x-value.
	Note that if not is true, then this will tally those different from pv."
	| cm slice countBlt copyBlt |
	cm := self newColorMap.		"Map all colors but pv to zero"
	not ifTrue: [cm atAllPut: 1].		"... or all but pv to one"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice := Form extent: 1@height.
	copyBlt := (BitBlt destForm: slice sourceForm: self
				halftoneForm: nil combinationRule: Form over
				destOrigin: 0@0 sourceOrigin: 0@0 extent: 1 @ slice height
				clipRect: slice boundingBox) colorMap: cm.
	countBlt := (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (0@0 extent: slice extent);
				combinationRule: 32.
	^ (0 to: width-1) collect:
		[:x |
		copyBlt sourceOrigin: x@0; copyBits.
		countBlt copyBits]