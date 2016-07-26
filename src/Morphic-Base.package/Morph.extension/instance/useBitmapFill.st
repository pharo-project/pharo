*Morphic-Base
useBitmapFill
	"Make receiver use a solid fill style (e.g., a simple color)"
	| fill |
	self fillStyle isBitmapFill ifTrue:[^self]. "Already done"
	fill := BitmapFillStyle fromForm: self defaultBitmapFillForm.
	"Note: Must fix the origin due to global coordinates"
	fill origin: self bounds origin.
	self fillStyle: fill.