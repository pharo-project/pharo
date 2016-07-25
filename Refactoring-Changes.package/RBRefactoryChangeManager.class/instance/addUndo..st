public access
addUndo: aRefactoringChange
	undo addLast: aRefactoringChange.
	undo size > UndoSize
		ifTrue: [ undo removeFirst ].
	redo := OrderedCollection new