masking
setMask: aShape
	"set mask pixels to 1 for all pixels covered by shape, aShape"

	^ aShape paintFillsUsing: surface setMaskPaint on: self
