drawing-images
warpImage: aForm transform: aTransform at: extraOffset sourceRect: sourceRect cellSize: cellSize
	"Warp the given using the appropriate transform and offset."
	| tfm |
	tfm := (MatrixTransform2x3 withOffset: origin) composedWithLocal: aTransform.
	^self privateWarp: aForm transform: tfm at: extraOffset sourceRect: sourceRect cellSize: cellSize