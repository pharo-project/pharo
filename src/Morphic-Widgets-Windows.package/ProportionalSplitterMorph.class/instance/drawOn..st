drawing
drawOn: aCanvas 

	| dotBounds size alphaCanvas dotSize |
	self shouldDraw ifFalse: [^self].
	super drawOn: aCanvas.
	self class showSplitterHandles ifTrue: [
	size := self splitsTopAndBottom
				ifTrue: [self handleSize transposed]
				ifFalse: [self handleSize].
	dotSize := self splitsTopAndBottom
				ifTrue: [6 @ self class splitterWidth]
				ifFalse: [self class splitterWidth @ 6].
	alphaCanvas := aCanvas asAlphaBlendingCanvas: 0.7.
	dotBounds := Rectangle center: self bounds center extent: size.
	alphaCanvas fillRectangle: dotBounds color: self handleColor.
	dotBounds := Rectangle center: self bounds center extent: dotSize.
	alphaCanvas fillRectangle: dotBounds color: self dotColor]