drawing-images
warpImage: aForm transform: aTransform at: extraOffset
	"Warp the given form using aTransform.
	TODO: Use transform to figure out appropriate cell size"
	^self warpImage: aForm transform: aTransform at: extraOffset sourceRect: aForm boundingBox cellSize: 1