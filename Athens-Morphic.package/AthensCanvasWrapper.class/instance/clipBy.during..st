canvas drawing-support
clipBy: aRectangle during: aBlock
	self canvasClipBy: aRectangle during: [ aBlock value: self ]