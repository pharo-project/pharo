populating browser
defaultMenuActions

	^ { 
		{ 'Redo changes'.
		#redoSelection.
		'Redo all code changes in selection (oldest first)'.
		#smallRedoIcon.
		$r command }.

		{ 'Undo changes'.
		#undoSelection.
		'Undo all code changes in selection (most recent first)'.
		#smallUndoIcon.
		$u command }.

		{ 'Comment'. 
		#commentSelection.
		'Edit comment of selected entries'.
		#announcementIcon. 
		$c command } 
	}