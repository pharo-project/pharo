menu
codeChangesMenuActions

	^ { 
		{ 'Apply...'.
		#openPreviewToApplySelectedChanges.
		'Preview the application of selected code changes (open a new window to show the effect of re-applying the changes in current image)'.
		#smallRedoIcon.
		$p command }.
		{ 'Revert...'.
		#openPreviewToRevertSelectedChanges.
		'Preview the reversion of selected code changes (open a new window to show the effect of reverting the changes in current image)'.
		#smallUndoIcon.
		$r command }.
		{ 'Browse current'.
		#browseSelection.
		'Browse working copy of entity affected in the selected event(s)'.
		#nautilusIcon.
		$b command }.
	}
