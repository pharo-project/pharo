drawing-support
translateBy: delta during: aBlock
	"Set a translation only during the execution of aBlock."
	| oldCanvas |
	oldCanvas := myCanvas.
	myCanvas translateBy: delta during:[:newCanvas|
		myCanvas := newCanvas.
		aBlock value: self].
	myCanvas := oldCanvas.