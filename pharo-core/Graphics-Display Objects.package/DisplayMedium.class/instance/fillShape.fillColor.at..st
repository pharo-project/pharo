coloring
fillShape: aShapeForm fillColor: aColor at: location
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	((BitBlt destForm: self sourceForm: aShapeForm fillColor: aColor
		combinationRule: Form paint
		destOrigin: location + aShapeForm offset sourceOrigin: 0@0
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits