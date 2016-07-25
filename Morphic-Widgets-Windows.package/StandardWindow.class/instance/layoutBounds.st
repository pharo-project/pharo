layout
layoutBounds
	"Bounds of pane area only."
	
	^self isFullscreen
		ifTrue: [self perform: #layoutBounds withArguments: #() inSuperclass: Morph]
		ifFalse: [super layoutBounds]