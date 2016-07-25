fill-styles
paneColorFor: aWindow
	"Answer the pane color that should be used."
	
	^self settings fadedBackgroundWindows
		ifTrue: [aWindow fillStyleToUse asColor]
		ifFalse: [(self windowActiveFillStyleFor: aWindow) asColor]