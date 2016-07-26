mouse events
mouseOverBorderStyle
	"Answer the border style to use whilst the mouse
	is over the receiver."

	^self valueOfProperty: #mouseOverBorderStyle ifAbsent: [BorderStyle thinGray]