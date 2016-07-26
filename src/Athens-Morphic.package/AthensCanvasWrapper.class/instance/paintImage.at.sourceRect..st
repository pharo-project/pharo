canvas drawing-images
paintImage: aForm at: aPoint sourceRect: sourceRect
	self canvas paintMode over.
	self imagePaint: aForm at: aPoint sourceRect: sourceRect