accessing
editor
	"Return my current editor, or install a new one."
	editor ifNotNil: [^ editor].
	^ self installEditorToReplace: nil