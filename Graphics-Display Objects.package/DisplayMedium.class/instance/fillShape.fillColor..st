coloring
fillShape: aShapeForm fillColor: aColor
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	^ self fillShape: aShapeForm fillColor: aColor at: 0@0