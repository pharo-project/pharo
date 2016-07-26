drawing
drawSubmorphsOn: aCanvas 
	"Draw the focus here since we are using inset bounds
	for the focus rectangle."
	"1haltOnce."
	super drawSubmorphsOn: aCanvas.
	self hasKeyboardFocus ifTrue: [ self drawKeyboardFocusOn: aCanvas ]
