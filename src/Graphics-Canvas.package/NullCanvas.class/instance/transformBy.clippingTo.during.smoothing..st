drawing-support
transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize
	"do this in order that timing runs work better"
	aBlock value: self