morph-drawing
fullDrawOn: aCanvas
	"fixes bug in canvas, when it tries to draw a morph even if clip rect has size 0"
	
	aCanvas clipRect hasPositiveExtent
		ifFalse: [ ^ self ].	
	
	super fullDrawOn: aCanvas