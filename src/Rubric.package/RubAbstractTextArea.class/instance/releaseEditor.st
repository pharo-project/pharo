accessing editor
releaseEditor
	"Release the editor for my paragraph."

	editor
		ifNotNil: [ 
			editor unplug.
			editor := nil ]