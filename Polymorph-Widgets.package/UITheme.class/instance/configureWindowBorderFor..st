border-styles
configureWindowBorderFor: aWindow
	"Configure the border for the given window."

	aWindow roundedCorners: #(1 2 3 4).
	aWindow borderStyle: (BorderStyle
		width: 1 color: Color lightGray)