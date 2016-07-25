border-styles
configureWindowDropShadowFor: aWindow
	"Configure the drop shadow for the given window."

	aWindow
		addDropShadow;
		shadowColor: self windowShadowColor;
		shadowOffset: 1@1