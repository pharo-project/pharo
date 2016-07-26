private - menu actions
redoSelectionCompleteRefactoringMenuAction

	^ { 'Redo (complete)'.
		#redoSelectionCompleteRefactoring.
		'Redo selected event(s)'.
		#smallRedoIcon.
		$r command }