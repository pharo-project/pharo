fill-styles
textEditorDisabledFillStyleFor: aTextEditor
	"Return the disabled fillStyle for the given text editor."
	
	^aTextEditor paneColor alphaMixed: 0.3 with: Color white