updating
resizerChanged
	self columns size > 1
		ifTrue: [
			self updateColumnResizersXOffset;
				resizeScroller;
				updateColumnMorphs]
		ifFalse: [self updateTopHeader]