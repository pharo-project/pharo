private - menu actions
redoSelectionPropagateRefactoringMenuAction

	^ { 'Redo (propagate)'.
		#redoSelectionPropagateRefactoring.
		'Redo the secondary effects of the selected refactoring'.
		#smallRedoIcon.
		$p command }