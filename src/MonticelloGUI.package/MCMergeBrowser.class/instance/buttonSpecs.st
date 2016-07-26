morphic ui
buttonSpecs
	^ #((Merge merge 'Proceed with the merge' canMerge)
		 (Cancel cancel 'Cancel the merge')
		('All Newer' chooseAllNewerConflicts 'Choose all newer conflict versions')
		('All Older' chooseAllOlderConflicts 'Choose all older conflict versions')
		('Rest Local' chooseAllUnchosenLocal 'Choose local versions of all remaining conflicts')
		('Rest Remote' chooseAllUnchosenRemote 'Choose remote versions of all remaining conflicts')
)