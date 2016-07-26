border-styles
configureWindowBorderFor: aWindow
	"Configure the border for the given window."

	super configureWindowBorderFor: aWindow.
	aWindow roundedCorners: #(1 4) "just top"