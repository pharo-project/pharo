masking
clearMask: aShape
	"set mask pixels to 0 for all pixels covered by shape, aShape"
	^ aShape paintFillsUsing: surface clearMaskPaint on: self
