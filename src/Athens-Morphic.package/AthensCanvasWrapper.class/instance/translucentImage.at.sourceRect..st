canvas drawing-images
translucentImage: aForm at: aPoint sourceRect: sourceRect
	self canvas paintMode over.
	self imagePaint: aForm at: aPoint sourceRect: sourceRect
