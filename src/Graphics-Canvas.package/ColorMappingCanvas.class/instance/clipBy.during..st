drawing-support
clipBy: aRectangle during: aBlock
	"Set a clipping rectangle active only during the execution of aBlock.
	Note: In the future we may want to have more general clip shapes - not just rectangles"
	| oldCanvas |
	oldCanvas := myCanvas.
	myCanvas clipBy: aRectangle during:[:newCanvas|
		myCanvas := newCanvas.
		aBlock value: self].
	myCanvas := oldCanvas