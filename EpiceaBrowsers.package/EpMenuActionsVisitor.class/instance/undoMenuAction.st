private - menu actions
undoMenuAction

	^ { 'Undo'.
		#undoSelection.
		'Do the inverse of the selected event(s)'.
		#smallUndoIcon.
		$u command }