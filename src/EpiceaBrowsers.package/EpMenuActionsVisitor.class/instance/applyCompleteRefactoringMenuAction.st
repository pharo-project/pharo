private - menu actions
applyCompleteRefactoringMenuAction

	^ { 'Apply refactoring'.
		#applyCompleteRefactoringInSelection.
		'Apply selected refactoring'.
		#smallRedoIcon.
		$y command }