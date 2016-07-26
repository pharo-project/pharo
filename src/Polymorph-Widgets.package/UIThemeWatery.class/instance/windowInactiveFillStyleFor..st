fill-styles
windowInactiveFillStyleFor: aWindow
	"Return the window inactive fillStyle for the given window."
	
	^self settings fadedBackgroundWindows
		ifTrue: [self windowColor muchLighter]
		ifFalse: [self windowColor]
