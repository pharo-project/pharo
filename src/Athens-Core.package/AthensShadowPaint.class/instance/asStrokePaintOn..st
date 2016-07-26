converting
asStrokePaintOn: aCanvas
	"receiver can be converted to stroke paint
	presering color and width"
	^ (aCanvas surface createStrokePaintFor: self color)
		width: self width