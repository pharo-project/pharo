border-styles
windowPaneBorderStyleFor: aMorph in: aSystemWindow
	"Answer the border style for a morph that is to be added
	as a pane in the given system window,"

	^BorderStyle simple
		color: aSystemWindow paneColor;
		width: (aMorph borderWidth = 0 ifTrue: [0] ifFalse: [1])