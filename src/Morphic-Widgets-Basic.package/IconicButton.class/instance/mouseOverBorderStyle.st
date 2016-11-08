mouse events
mouseOverBorderStyle
	"Answer the border style to use whilst the mouse
	is over the receiver."

	^mouseOverBorderStyle ifNil: [BorderStyle thinGray]