events
focused: aBoolean from: aTextMorph
	(aBoolean and: [aTextMorph sharesFindReplace])
		ifTrue: [[(EditorFindReplaceDialogWindow on: aTextMorph) comeToFront] on: Error do: []].