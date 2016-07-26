fill-styles
windowInactiveLabelFillStyleFor: aWindow
	"Return the window inactive label fillStyle for the given window."
	
	^self settings fadedBackgroundWindows
		ifTrue: [aWindow paneColorToUse alphaMixed: 0.6 with: Color black]
		ifFalse: [self windowActiveLabelFillStyleFor: aWindow]