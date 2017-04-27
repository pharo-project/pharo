menu
refactoringMenuActions

	^ {
		{ 'Apply refactoring'.
		#applyCompleteRefactoringInSelection.
		'Apply selected refactoring'.
		#smallRedoIcon.
		$y command }.
		{ 'Apply secondary effects of refactoring'.
		#applyPropagateRefactoringInSelection.
		'Apply only the secondary effects of the selected refactoring'.
		#smallRedoIcon.
		$s command }
	}