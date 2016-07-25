geometry
privateMoveBy: delta 
	super privateMoveBy: delta.
	editor ifNil: [paragraph ifNotNil: [paragraph moveBy: delta]]
		ifNotNil: 
			["When moving text with an active editor, save and restore all state."

			paragraph moveBy: delta.
			self installEditorToReplace: editor]