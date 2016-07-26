drawing
drawSubmorphsOn: aCanvas 
	"Display submorphs back to front.
	Draw the focus here since we are using inset bounds
	for the focus rectangle."

	super drawSubmorphsOn: aCanvas.
	self hasKeyboardFocus ifTrue: [self drawKeyboardFocusOn: aCanvas]