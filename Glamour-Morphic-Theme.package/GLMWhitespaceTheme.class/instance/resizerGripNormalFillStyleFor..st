fill-styles
resizerGripNormalFillStyleFor: aResizer
	"Return the normal fillStyle for the given resizer.
	For the moment, answer a transparent colour for no drawing,
	non transparent to draw as normal."
	self flag: 'this is used for the edge grip'.
	^ super resizerGripNormalFillStyleFor: aResizer
	
"	aResizer 
		instVarNamed: #handleColor put: (self glamorousDarkBaseColorFor:  aResizer);
		instVarNamed: #dotColor put: Color transparent.
	
	^SolidFillStyle color: Color red"