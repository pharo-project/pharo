drawing-images
warpImage: aForm transform: aTransform at: extraOffset sourceRect: sourceRect cellSize: cellSize
	"Warp the given using the appropriate transform and offset."
	^self subclassResponsibility