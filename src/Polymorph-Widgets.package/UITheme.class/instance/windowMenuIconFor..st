label-styles
windowMenuIconFor: aWindow
	"Answer the menu icon for the given window."

	^ aWindow taskbarIcon ifNil: [self windowMenuForm]