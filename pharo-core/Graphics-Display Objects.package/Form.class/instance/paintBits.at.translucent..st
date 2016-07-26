displaying
paintBits: sourceForm at: destOrigin translucent: factor
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt destForm: self
		sourceForm: sourceForm
		halftoneForm: nil
		combinationRule: 31
		destOrigin: destOrigin
		sourceOrigin: 0@0
		extent: sourceForm extent
		clipRect: self boundingBox)
		copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f := Form fromUser. f replaceColor: f peripheralColor withColor: Color transparent.
f2 := Form fromDisplay: (0@0 extent: f extent). f3 := f2 deepCopy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 := f2 deepCopy. f3 paintBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"