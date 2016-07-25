other
translateBy: delta clippingTo: aRectangle during: aBlock
	"Set a translation and clipping rectangle only during the execution of aBlock."
	| oldCanvas |
	oldCanvas := myCanvas.
	myCanvas translateBy: delta clippingTo: aRectangle during:[:newCanvas|
		myCanvas := newCanvas.
		aBlock value: self].
	myCanvas := oldCanvas.