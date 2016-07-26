drawing-support
preserveStateDuring: aBlock
	"Preserve the full canvas state during the execution of aBlock"
	| oldCanvas result |
	oldCanvas := myCanvas.
	result := myCanvas preserveStateDuring:[:newCanvas|
		myCanvas := newCanvas.
		aBlock value: self].
	myCanvas := oldCanvas.
	^result