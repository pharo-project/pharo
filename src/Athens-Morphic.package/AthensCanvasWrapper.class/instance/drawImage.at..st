canvas drawing-images
drawImage: aForm at: aPoint
	"Draw the given Form, which is assumed to be a Form or ColorForm"

	self drawImage: aForm
		at: aPoint
		sourceRect: aForm boundingBox