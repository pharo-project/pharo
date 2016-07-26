copying
copyClipRect: aRectangle
	^ self copyOrigin: origin clipRect: (aRectangle translateBy: origin)
