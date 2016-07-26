drawing-support
translateTo: newOrigin clippingTo: aRectangle during: aBlock
	"Set a new origin and clipping rectangle only during the execution of aBlock."
	| oldCanvas |
	oldCanvas := myCanvas.
	myCanvas translateTo: newOrigin clippingTo: aRectangle during:[:newCanvas|
		myCanvas := newCanvas.
		aBlock value: self].
	myCanvas := oldCanvas.