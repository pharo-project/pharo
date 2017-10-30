border-styles
scrollPaneNormalBorderStyleFor: aScrollPane
	"Return the normal borderStyle for the given scroll pane."

	^BorderStyle simple
		width: 1;
		baseColor: aScrollPane paneColor