private
defaultMenuActions

	^ { 
		{ 'Apply changes'.
		#applyChangesInSelection.
		'Apply all code changes in selection (oldest first)'.
		#smallRedoIcon.
		$p command }.

		{ 'Revert changes'.
		#revertChangesInSelection.
		'Revert all code changes in selection (most recent first)'.
		#smallUndoIcon.
		$r command }.

		{ 'Comment'. 
		#commentSelection.
		'Edit comment of selected entries'.
		#announcementIcon. 
		$c command } 
	}