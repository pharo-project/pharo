rendering
renderShadowOf: aBrick on: aCanvas

	"no need to even try drawing if shadow width is zero"
	aBrick shadowWidth = 0 ifTrue: [ ^ self ].
	
	self
		topLeftCornerShadowOf: aBrick on: aCanvas;
		topRightCornerShadowOf: aBrick on: aCanvas;
		bottomRightCornerShadowOf: aBrick on: aCanvas;
		bottomLeftCornerShadowOf: aBrick on: aCanvas;
		leftShadowOf: aBrick on: aCanvas;
		topShadowOf: aBrick on: aCanvas;
		rightShadowOf: aBrick on: aCanvas;
		bottomShadowOf: aBrick on: aCanvas
