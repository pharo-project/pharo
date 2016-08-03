private - menu actions
redoSelectionPropagateRefactoringMenuAction

	^ { 'Redo secondary effects of refactoring'.
		#redoSelectionPropagateRefactoring.
		'Redo the secondary effects of the selected refactoring'.
		#smallRedoIcon.
		$p command }