border-styles
textEditorNormalBorderStyleFor: aTextEditor
	"Return the normal text editor borderStyle for the given text editor."

	^BorderStyle inset
		width: 1;
		baseColor: aTextEditor paneColor